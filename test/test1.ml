open Core
open Bistro

let add x y = Workflow.cached_value (fun%workflow () ->
    [%eval x] + [%eval y]
  )

let%workflow mul x y : int workflow =
  [%eval x] * [%eval y]

let pipeline = add (Workflow.int 1) (Workflow.int 41)

let _ =
  let open Bistro_engine in
  let open Lwt_result.Infix in
  let db = Db.init_exn "_bistro" in
  Scheduler.eval db pipeline
  >|= Printf.printf "%d\n"
  |> Lwt_main.run
