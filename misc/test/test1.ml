open Core
open Bistro

let add x y = Workflow.plugin ~descr:"add" (fun%workflow () ->
    [%eval x] + [%eval y]
  )

let mul x y : int workflow =
  Workflow.plugin ~descr:"mul" (fun%workflow () ->
      [%eval x] * [%eval y]
    )
[@@ocaml.warning "-32"]

let pipeline = add (Workflow.int 1) (Workflow.int 41)

let () =
  let open Bistro_engine in
  let db = Db.init_exn "_bistro" in
  let sched = Scheduler.create db in
  let thread = Scheduler.eval_exn sched pipeline in
  Scheduler.start sched ;
  Printf.printf "%d\n" (Lwt_main.run thread)
