type time = float

type t =
  | Run of { ready : time ;
             start : time ;
             _end_ : time ;
             outcome : Task_result.t }

  | Skipped of [ `Done_already
               | `Missing_dep
               | `Allocation_error of string ]

val is_errored : t -> bool

val run :
  ready:time ->
  start:time ->
  _end_:time ->
  outcome:Task_result.t ->
  t

val skipped :
  [ `Done_already
  | `Missing_dep
  | `Allocation_error of string ] -> t

val error_report :
  t ->
  Db.t ->
  Buffer.t ->
  string ->
  unit

