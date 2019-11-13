open Bistro

let f x y =
  Workflow.plugin ~descr:"add" (
    let%workflow x = x
    and          y = y in
    x + y
  )
