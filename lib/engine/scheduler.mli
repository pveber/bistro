open Bistro_base

type t

type time = float

type event =
  | Start
  | End


type logger = time -> event -> unit

val create :
  ?loggers:logger list ->
  ?np:int ->
  ?mem:[`GB of int] ->
  ?use_docker:bool ->
  Db.t -> t

val submit :
  t -> _ Workflow.t -> Execution_trace.t Lwt.t

val eval_expr :
  t -> 'a Workflow.expr -> ('a, unit) result Lwt.t

val start : t -> unit

val join : t -> unit Lwt.t
