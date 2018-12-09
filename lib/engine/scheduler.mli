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

type 'a t

val create :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?use_docker:bool ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  Db.t ->
  'a Bistro.workflow ->
  'a t

val run :
  'a t ->
  ('a, Execution_trace.t list) Lwt_result.t

val gc_state : _ t -> Gc.state option

val eval :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?use_docker:bool ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  Db.t ->
  'a Bistro.workflow ->
  ('a, Execution_trace.t list) Lwt_result.t

val eval_exn :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?use_docker:bool ->
  ?loggers:Logger.t list ->
  ?collect:bool ->
  Db.t ->
  'a Bistro.workflow ->
  'a Lwt.t

val error_report :
  Db.t ->
  Execution_trace.t list ->
  string
