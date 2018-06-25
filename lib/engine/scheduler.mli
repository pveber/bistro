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
  db:Db.t ->
  use_docker:bool ->
  unit -> t

val submit :
  t -> _ Workflow.t -> unit

val start : t -> unit Lwt.t

val join :
  t ->
  (string * Execution_trace.t) list Lwt.t
