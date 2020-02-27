open Bistro

let f x y =
  Workflow.plugin ~descr:"add" (
    let%deps x = x
    and      y = y in
    x + y
  )
