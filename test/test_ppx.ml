open Bistro

let f x y =
  Workflow.plugin ~descr:"add" (fun%workflow () ->
    [%eval x] + [%eval y]
  )

let%workflow [@np 1] [@mem Workflow.int 300] g x y = [%eval x] + [%eval y]
