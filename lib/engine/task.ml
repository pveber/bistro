open Core
open Lwt
open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t = Workflow.t

let step_outcome ~exit_code ~dest_exists=
  match exit_code, dest_exists with
    0, true -> `Succeeded
  | 0, false -> `Missing_output
  | _ -> `Failed

let requirement = function
  | Workflow.Input _
  | Select _ ->
    Allocator.Request { np = 0 ; mem = 0 }
  | Plugin { np ; mem ; _ }
  | Shell { np ; mem ; _ } ->
    Allocator.Request { np ; mem }


let select_path db dir q =
  let p = Db.path db dir in
  let q = Path.to_string q in
  Filename.concat p q

let rec waitpid pid =
  try Unix.waitpid pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid


let rec expand_collection db : Workflow.t_list -> Workflow.t list Lwt.t = function
  | List l -> Lwt.return l.elts
  | Glob g ->
    let dir_path = Db.path db g.dir in
    Misc.files_in_dir dir_path >>= fun files ->
    let ws = List.map files ~f:(fun fn -> Workflow.select g.dir [fn]) in
    Lwt.return ws
  | ListMap lm ->
    expand_collection db lm.elts >>= fun ws ->
    Lwt.return (List.map ws ~f:lm.f)

let rec expand_template db =
  let open Template in
  let ret1 x = Lwt.return [ x ] in
  function
  | S s -> ret1 (S s)
  | DEST -> ret1 DEST
  | TMP -> ret1 TMP
  | NP -> ret1 NP
  | MEM -> ret1 MEM
  | F tmpl ->
    Lwt_list.map_p (expand_template db) tmpl >>= fun tmpl ->
    ret1 (Template.F (List.concat tmpl))
  | D (Workflow.WDepT w) -> ret1 (D w)
  | D (Workflow.WLDepT (ws, sep)) ->
    expand_collection db ws >>= fun ws ->
    List.map ws ~f:(fun w -> Template.D w)
    |> List.intersperse ~sep:(S sep)
    |> Lwt.return

let rec expand_command db =
  let open Command in
  function
  | Simple_command c ->
    Lwt_list.map_p (expand_template db) c >|= List.concat >>= fun c ->
    Lwt.return (Simple_command c)
  | And_list xs ->
    Lwt_list.map_p (expand_command db) xs >>= fun xs ->
    Lwt.return (And_list xs)
  | Or_list xs ->
    Lwt_list.map_p (expand_command db) xs >>= fun xs ->
    Lwt.return (Or_list xs)
  | Pipe_list xs ->
    Lwt_list.map_p (expand_command db) xs >>= fun xs ->
    Lwt.return (Pipe_list xs)
  | Docker (img, c) ->
    expand_command db c >>= fun c ->
    Lwt.return (Docker (img, c))

let perform t config (Allocator.Resource { np ; mem }) =
  match t with
  | Workflow.Input { path ; id } ->
    let pass = Sys.file_exists path = `Yes in
    (
      if pass then Misc.cp path (Db.cache config.db id)
      else Lwt.return ()
    ) >>= fun () ->
    Lwt.return (Task_result.Input { id ; path ; pass })

  | Select { id ; dir ; sel } ->
    Lwt.wrap (fun () ->
        let p = select_path config.db dir sel in
        let pass = Sys.file_exists p = `Yes in
        Task_result.Select {
          id ;
          pass ;
          dir_path = Db.path config.db dir ;
          sel ;
        }
      )

  | Shell { task = cmd ; id ; descr ; _ } ->
    let env =
      Execution_env.make
        ~use_docker:config.use_docker
        ~db:config.db
        ~np ~mem ~id
    in
    expand_command config.db cmd >|= Shell_command.make env >>= fun cmd ->
    Shell_command.run cmd >>= fun (exit_code, dest_exists) ->
    let cache_dest = Db.cache config.db id in
    let outcome = step_outcome ~exit_code ~dest_exists in
    Misc.(
      if outcome = `Succeeded then
        mv env.dest cache_dest >>= fun () ->
        remove_if_exists env.tmp_dir
      else
        Lwt.return ()
    ) >>= fun () ->
    Lwt.return (Task_result.Shell {
        outcome ;
        id ;
        descr ;
        exit_code ;
        cmd = Shell_command.text cmd ;
        file_dumps = Shell_command.file_dumps cmd ;
        cache = if outcome = `Succeeded then Some cache_dest else None ;
        stdout = env.stdout ;
        stderr = env.stderr ;
      })

  | Plugin { task = f ; id ; descr ; _ } ->
    let env =
      Execution_env.make
        ~use_docker:config.use_docker
        ~db:config.db
        ~np ~mem ~id
    in
    let obj_env = object
      method np = env.np
      method mem = env.mem
      method tmp = env.tmp
      method dest = env.dest
    end
    in
    Misc.touch env.stdout >>= fun () ->
    Misc.touch env.stderr >>= fun () ->
    let (read_from_child, write_to_parent) = Unix.pipe () in
    let (read_from_parent, write_to_child) = Unix.pipe () in
    Misc.remove_if_exists env.tmp_dir >>= fun () ->
    Unix.mkdir_p env.tmp ;
    match Unix.fork () with
    | `In_the_child ->
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      let exit_code =
        try f obj_env ; 0
        with e ->
          Out_channel.with_file env.stderr ~f:(fun oc ->
              fprintf oc "%s\n" (Exn.to_string e) ;
              Printexc.print_backtrace oc
            ) ;
          1
      in
      let oc = Unix.out_channel_of_descr write_to_parent in
      Marshal.to_channel oc exit_code [] ;
      Caml.flush oc ;
      Unix.close write_to_parent ;
      ignore (Caml.input_value (Unix.in_channel_of_descr read_from_parent)) ;
      assert false
    | `In_the_parent pid ->
      Unix.close write_to_parent ;
      Unix.close read_from_parent ;
      let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input read_from_child in
      Lwt_io.read_value ic >>= fun (exit_code : int) ->
      Caml.Unix.kill (Pid.to_int pid) Caml.Sys.sigkill;
      ignore (waitpid pid) ;
      Unix.close read_from_child ;
      Unix.close write_to_child ;
      let cache_dest = Db.cache config.db id in
      let dest_exists = Sys.file_exists env.dest = `Yes in
      let outcome = step_outcome ~dest_exists ~exit_code in
      Misc.(
        if outcome = `Succeeded then
          mv env.dest cache_dest >>= fun () ->
          remove_if_exists env.tmp_dir
        else
          Lwt.return ()
      ) >>= fun () ->
      Lwt.return (Task_result.Plugin {
          id ;
          descr ;
          outcome ;
        })

let is_done t db =
  let path = match t with
    | Workflow.Input { id ; _ } -> Db.cache db id
    | Select { dir ; sel ; _ } -> select_path db dir sel
    | Shell { id ; _ } -> Db.cache db id
    | Plugin { id ; _ } -> Db.cache db id
  in
  Lwt.return (Sys.file_exists path = `Yes)
