open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

val requirement : Workflow.t -> Allocator.request

val perform : Workflow.t -> config -> Allocator.resource -> Task_result.t Lwt.t

val is_done : Workflow.t -> Db.t -> bool Lwt.t
