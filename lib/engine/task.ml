open Core
open Lwt
open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t =
  | Input of { id : string ; path : string }
  | Select of {
      id : string ;
      dir : Workflow.t ;
      sel : string list
    }
  | Shell of {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : Workflow.t Command.t ;
    }
  | Plugin of {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      f : Workflow.env -> unit ;
    }
  | MapDir of {
      id : string ;
      targets : Workflow.t list ;
      names : string list ;
    }

let input ~id ~path = Input { id ; path }
let select ~id ~dir ~sel = Select { id ; dir ; sel }
let shell ~id ~descr ~np ~mem cmd = Shell { id ; cmd ; np ; mem ; descr }
let plugin ~id ~descr ~np ~mem f = Plugin { id ; f ; np ; mem ; descr }
let mapdir ~id ~targets ~names = MapDir { id ; targets ; names }

let step_outcome ~exit_code ~dest_exists=
  match exit_code, dest_exists with
    0, true -> `Succeeded
  | 0, false -> `Missing_output
  | _ -> `Failed

let requirement = function
  | Input _
  | Select _
  | MapDir _ ->
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

let perform t config (Allocator.Resource { np ; mem }) =
  match t with
  | Input { path ; id } ->
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

  | Shell { cmd ; id ; descr ; _ } ->
    let env =
      Execution_env.make
        ~use_docker:config.use_docker
        ~db:config.db
        ~np ~mem ~id
    in
    let cmd = Shell_command.make env cmd in
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

  | Plugin { f ; id ; descr ; _ } -> (
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
    )
  | MapDir md ->
    let dest = Db.cache config.db md.id in
    let move (w, fn) =
      let src = Db.path config.db w in
      let dst = Filename.concat dest fn in
      Lwt_unix.rename src dst
    in
    Unix.mkdir_p dest ;  (* FIXME: blocking *)
    Lwt_list.iter_p move (List.zip_exn md.targets md.names) >|= fun () ->
    Task_result.MapDir { id = md.id ; pass = true }

(* let normalized_repo_items ?(base = "") ?ext repo_path results =
 *   List.mapi results ~f:(fun i (w, cache_path) ->
 *       let fn = sprintf "%s%06d%s" base i (match ext with None -> "" | Some e -> "." ^ e) in
 *       let repo_path = repo_path @ [ fn ] in
 *       normalized_repo_item repo_path w cache_path
 *     )
 *   |> List.concat *)


let is_done t db =
  let path = match t with
    | Workflow.Input { id ; _ }
    | Shell { id ; _ }
    | Plugin { id ; _ }
    | MapDir { id ; _ } -> Db.cache db id
    | Select { dir ; sel ; _ } -> select_path db dir sel
  in
  Lwt.return (Sys.file_exists path = `Yes)
