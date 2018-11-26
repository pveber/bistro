open Core
open Bistro
open Lwt_result.Infix

let add x y = cached_value (fun%bistro () ->
    [%eval x] + [%eval y]
  )

let pipeline = add (int 1) (int 41)

let _ =
  Bistro_engine.Scheduler.eval pipeline >|= Printf.printf "%d\n"
  |> Lwt_main.run
