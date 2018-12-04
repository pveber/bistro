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

val error_report :
  Db.t ->
  Execution_trace.t list ->
  string
