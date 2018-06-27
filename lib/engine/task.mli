open Bistro_base

type config = {
  db : Db.t ;
  use_docker : bool ;
}

type t = private
  | Input of { id : string ; path : string }
  | Select of {
      dir : Workflow.u ;
      sel : string list
    }
  | Shell of {
      id : string ;
      descr : string ;
      np : int ;
      mem : int ;
      cmd : Workflow.dep Command.t ;
    }

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
