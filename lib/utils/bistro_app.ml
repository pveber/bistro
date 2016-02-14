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
  | `Ok cache_path ->
    link dest cache_path ;
    `Ok ()
  | `Error xs ->
    `Error (
      List.map xs ~f:(fun (u, msg) ->
          Bistro.Workflow.id' u,
          msg,
          Db.report db u
        )
    )

let error_report = function
  | `Ok () -> ()
  | `Error xs ->
    List.iter xs ~f:(fun (_,_,report) ->
        prerr_endline report
      )

let simple ?(np = 1) ?(mem = 1024) targets =
  Db.with_open_exn "_bistro" (fun db ->
      let backend = Scheduler.local_backend ~np ~mem in
      let scheduler = Scheduler.make backend db in
      Lwt_list.map_p (foreach_target db scheduler) targets >>= fun results ->
      List.iter results ~f:error_report ;
      return ()
    )
  |> Lwt_unix.run
