open Bistro

let add x y =
  Workflow.plugin ~descr:"add" (fun%workflow () ->
    [%eval x] + [%eval y]
  )
[@@ocaml.warning "-32"]

let mul x y =
  let f = fun%workflow () ->
    [%eval x] * [%eval y]
  in
  Workflow.plugin ~descr:"mul" f
[@@ocaml.warning "-32"]
