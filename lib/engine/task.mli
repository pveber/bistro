open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t = Workflow.t

val requirement : t -> Allocator.request

val perform : t -> config -> Allocator.resource -> Task_result.t Lwt.t

val is_done : t -> Db.t -> bool Lwt.t
