open Bistro

let f x y =
  Workflow.plugin ~descr:"add" (
    let%deps x = x
    and      y = y in
    x + y
  )

let%workflow [@np 1] [@mem Workflow.int 300] g x y = [%eval x] + [%eval y]
