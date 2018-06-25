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
      dir : Workflow.u ;
      sel : string list
    }
  | Shell of {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : Workflow.dep Command.t ;
    }

let input ~id ~path = Input { id ; path }
let select ~dir ~sel = Select { dir ; sel }
let shell ~id ~descr ~np ~mem cmd = Shell { id ; cmd ; np ; mem ; descr }

let step_outcome ~exit_code ~dest_exists=
  match exit_code, dest_exists with
    0, true -> `Succeeded
  | 0, false -> `Missing_output
  | _ -> `Failed

let requirement = function
  | Input _
  | Select _ ->
    Allocator.Request { np = 0 ; mem = 0 }
  | Shell { np ; mem ; _ } ->
    Allocator.Request { np ; mem }


let select_path db dir q =
  let p = Db.path db dir in
  let q = Path.to_string q in
  Filename.concat p q

let perform t config (Allocator.Resource { np ; mem }) =
  match t with
  | Input { path ; id } ->
    let pass = Sys.file_exists path = `Yes in
    (
      if pass then Misc.ln path (Db.cache config.db id)
      else Lwt.return ()
    ) >>= fun () ->
    Lwt.return (Task_result.Input { path ; pass })

  | Select { dir ; sel } ->
    Lwt.wrap (fun () ->
        let p = select_path config.db dir sel in
        let pass = Sys.file_exists p = `Yes in
        Task_result.Select {
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


(* let perform_map_dir db ~files_in_dir ~goals w =
 *   let dest = Workflow.path db w in
 *   Unix.mkdir_p dest ;
 *   List.map2_exn files_in_dir goals ~f:(fun fn g ->
 *       let dest = Filename.concat dest fn in
 *       match (g : Workflow.t) with
 *       | Input { path ; _ } ->
 *         Misc.ln path dest
 *       | (Select _ | Shell _ | Map_dir _) ->
 *         Misc.mv (Workflow.path db g) dest
 *     ) |> Lwt.join >>= fun () ->
 *   Lwt.return (
 *     `Map_dir {
 *       Task_result.Map_dir.pass = true ;
 *       cache = Some dest ;
 *     }
 *   ) *)

let is_done : type s. s Workflow.t -> Db.t -> bool Lwt.t = fun t db ->
  let open Workflow in
  let path = match t with
    | Input { path ; _ } -> path
    | Select { dir ; sel ; _ } -> select_path db dir sel
    | Shell { id ; _ } -> Db.cache db id
    | Closure { id ; _ } -> Db.cache db id
  in
  Lwt.return (Sys.file_exists path = `Yes)
