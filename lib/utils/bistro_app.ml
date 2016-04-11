open Core.Std
open Bistro.Std
open Bistro_engine
open Lwt

type target = string list * Bistro.Workflow.u
with sexp

type plan = target list with sexp

let load_plan fn =
  In_channel.read_all fn
  |> Sexp.of_string
  |> plan_of_sexp

let save_plan fn p =
  sexp_of_plan p
  |> Sexp.to_string_hum
  |> fun data -> Out_channel.write_all fn ~data

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

let foreach_target db scheduler outdir (dest, u) =
  Scheduler.build' scheduler u >|= function
  | Ok cache_path ->
    link (outdir :: dest) cache_path ;
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

let with_backend backend ~outdir targets =
  let main =
    let db = Db.init_exn "_bistro" in
    let scheduler = Scheduler.make backend db in
    Lwt_list.map_p (foreach_target db scheduler outdir) targets >>= fun results ->
    List.iter results ~f:error_report ;
    return ()
  in
  Lwt_unix.run main

let local ?(np = 1) ?(mem = 1024) ?tmpdir ~outdir targets =
  with_backend (Scheduler.local_backend ?tmpdir ~np ~mem ()) ~outdir targets
