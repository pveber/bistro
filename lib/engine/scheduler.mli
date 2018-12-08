type error = [
  | `Msg of string
]

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

module Gc : sig
  open Bistro_internals

  type t
  val fold_deps :
    t ->
    init:'a ->
    f:('a -> Workflow.any -> Workflow.any -> 'a) ->
    'a
end
