type error = [
  | `Msg of string
]


module Gc : sig
  open Bistro_internals

  type state = {
    deps : (Workflow.any * Workflow.any) list ;
    protected : Workflow.any list ;
  }
end

type t

val create :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  Db.t ->
  t

val gc_state : t -> Gc.state option

val start : t -> unit

val eval :
  t ->
  'a Bistro.workflow ->
  ('a, Execution_trace.t list) Lwt_result.t

val eval_exn :
  t ->
  'a Bistro.workflow ->
  'a Lwt.t

val error_report :
  Db.t ->
  Execution_trace.t list ->
  string

val stop : t -> unit Lwt.t

val simple_eval_exn :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  ?db_path:string ->
  'a Bistro.workflow ->
  'a
