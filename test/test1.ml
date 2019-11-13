open Core
open Bistro

let add x y = Workflow.plugin (fun%workflow_expr () ->
    [%eval x] + [%eval y]
  )

let%workflow mul x y : int workflow =
  [%eval x] * [%eval y]

let pipeline = add (Workflow.int 1) (Workflow.int 41)

let () =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let sched = Scheduler.create db in
  let thread = Scheduler.eval_exn sched pipeline in
  Scheduler.start sched ;
  Printf.printf "%d\n" (Lwt_main.run thread)
