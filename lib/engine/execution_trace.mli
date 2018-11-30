type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Done_already of { id : string }
  | Canceled of {
      id : string ;
      missing_deps : t list ;
    }
  | Allocation_error of {
      id : string ;
      msg : string ;
    }

val is_errored : t -> bool

val error_report :
  t ->
  Db.t ->
  Buffer.t ->
  unit

val all_ok : t list -> bool

val gather_failures : t list -> t list
