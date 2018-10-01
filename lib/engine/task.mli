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
  id:string ->
  dir:Workflow.t ->
  sel:string list ->
  t

val shell :
  id:string ->
  descr:string ->
  np:int ->
  mem:int ->
  Workflow.t Command.t -> t

val plugin :
  id:string ->
  descr:string ->
  np:int ->
  mem:int ->
  (Workflow.env -> unit) -> t

val mapdir :
  id:string ->
  targets:Workflow.t list ->
  names:string list ->
  t

val requirement : t -> Allocator.request

val perform : t -> config -> Allocator.resource -> Task_result.t Lwt.t

val is_done : Workflow.t -> Db.t -> bool Lwt.t
