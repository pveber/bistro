open Core.Std
open Bistro.Std
open Bistro_engine
open Lwt

type target = Target : string list * _ workflow -> target

type plan = target list

let ( %> ) path w = Target (path, w)

let rec string_of_path = function
  | [] -> "."
  | "" :: t -> Filename.concat "." (string_of_path t)
  | p -> List.reduce_exn p ~f:Filename.concat

let make_absolute p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let link p p_u =
  let dst = string_of_path p in
  let src = make_absolute p_u in
  Unix.mkdir_p (Filename.dirname dst) ;
  let cmd = sprintf "rm -rf %s && ln -s %s %s" dst src dst in
  ignore (Sys.command cmd)

let foreach_target outdir (Target (dest, w)) = function
  | Ok cache_path ->
    link (outdir :: dest) cache_path ;
    Ok ()
  | Error xs -> Error xs

let error_report db = function
  | Ok () -> ()
  | Error xs ->
    List.iter xs ~f:(fun (dep, msg) ->
        match dep with
        | `Input i ->
          fprintf stderr "################################################################################\n" ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "#  Invalid input %s\n" (string_of_path i) ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "# %s\n" msg ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "################################################################################\n"

        | `Select (tid, p) ->
          fprintf stderr "################################################################################\n" ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "#  Invalid select: no %s in %s\n" (string_of_path p) tid ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "# %s\n" msg ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "################################################################################\n" ;

        | `Task tid ->
          let report = match Db.Task_table.get db tid with
            | Some t -> Db.report db t
            | None -> sprintf "Unregistered task %s" tid
          in
          fprintf stderr "################################################################################\n" ;
          fprintf stderr "#                                                                              #\n" ;
          fprintf stderr "#  Task %s failed\n" tid ;
          fprintf stderr "#                                                                               \n" ;
          fprintf stderr "#------------------------------------------------------------------------------#\n" ;
          fprintf stderr "#                                                                               \n" ;
          fprintf stderr "# %s\n" msg ;
          fprintf stderr "#                                                                              #\n" ;
          prerr_endline report
      )

let with_backend backend ~outdir targets =
  let main =
    let db = Db.init_exn "_bistro" in
    let scheduler = Scheduler.make backend db in
    let workflows = List.map targets ~f:(fun (Target (_, w)) -> Bistro.Workflow w) in
    Scheduler.build_all scheduler workflows >>= fun results ->
    List.map2_exn targets results ~f:(foreach_target outdir)
    |> List.iter ~f:(error_report db) ;
    return ()
  in
  Lwt_unix.run main

let local ?use_docker ?(np = 1) ?(mem = 1024) ?tmpdir ~outdir targets =
  with_backend (Scheduler.local_backend ?use_docker ?tmpdir ~np ~mem ()) ~outdir targets
