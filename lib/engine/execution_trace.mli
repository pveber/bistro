open Bistro_base

type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Done_already of Task.t
  | Canceled of {
      task : Task.t ;
      missing_deps : t list ;
    }
  | Allocation_error of Task.t * string
  | Invalid_glob of {
      dir : Workflow.t ;
    }

val is_errored : t -> bool

val error_report :
  t ->
  Db.t ->
  Buffer.t ->
  unit

val all_ok : t list -> bool

val gather_failures : t list -> t list
