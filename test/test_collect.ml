open Bistro
open Bistro.Shell_dsl
open Bistro_utils

let remove_e x =
  Workflow.shell ~descr:"remove_e" [
    cmd "sed" ~stdout:dest [ string "'s/e//g'" ; dep x ]
  ]

let w : directory pworkflow =
  Workflow.glob ~pattern:"*.mli" (Workflow.input "lib")
  |> Workflow.spawn ~f:remove_e
  |> Workflow.collect ~ext:"ml" ~prefix:"source"

module T = Toplevel_eval.Make(struct
    let np = 6
    let mem = 1
  end)()

let () =
  T.path w
  |> print_endline
