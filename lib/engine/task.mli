open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t

val input :
  id:string ->
  path:string ->
  t

val select :
  dir:_ Workflow.t ->
  sel:string list ->
  t

val shell :
  id:string ->
  descr:string ->
  np:int ->
  mem:int ->
  Workflow.dep Command.t -> t

val requirement : t -> Allocator.request

val perform : t -> config -> Allocator.resource -> Task_result.t Lwt.t

val is_done : _ Workflow.t -> Db.t -> bool Lwt.t
