open Bistro

let add x y =
  Workflow.plugin ~descr:"add" (fun%workflow () ->
    [%eval x] + [%eval y]
  )

let mul x y =
  let f = fun%workflow () ->
    [%eval x] * [%eval y]
  in
  Workflow.plugin ~descr:"mul" f
