open Core.Std
open Bistro.Std
open Bistro_engine
open Lwt

type target = string list * Bistro.Workflow.u

let ( %> ) path w = path, Bistro.Workflow.u w

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

let foreach_target db scheduler (dest, u) =
  Scheduler.build' scheduler u >|= function
  | Ok cache_path ->
    link dest cache_path ;
    Ok ()
  | Error xs ->
    Error (
      List.map xs ~f:(fun (u, msg) ->
          Bistro.Workflow.id' u,
          msg,
          Db.report db u
        )
    )

let error_report = function
  | Ok () -> ()
  | Error xs ->
    List.iter xs ~f:(fun (wid, msg, report) ->
        fprintf stderr "################################################################################\n" ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "#  Workflow %s failed\n" wid ;
        fprintf stderr "#                                                                               \n" ;
        fprintf stderr "#------------------------------------------------------------------------------#\n" ;
        fprintf stderr "#                                                                               \n" ;
        fprintf stderr "# %s\n" msg ;
        fprintf stderr "#                                                                              #\n" ;
        prerr_endline report
      )

let with_backend backend targets =
  let main =
    let db = Db.init_exn "_bistro" in
    let scheduler = Scheduler.make backend db in
    Lwt_list.map_p (foreach_target db scheduler) targets >>= fun results ->
    List.iter results ~f:error_report ;
    return ()
  in
  Lwt_unix.run main

let local ?(np = 1) ?(mem = 1024) ?workdir targets =
  with_backend (Scheduler.local_backend ?workdir ~np ~mem ()) targets
