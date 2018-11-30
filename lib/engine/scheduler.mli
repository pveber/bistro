type error = [
  | `Msg of string
]

val eval :
  ?use_docker:bool ->
  Db.t ->
  'a Bistro.workflow ->
  ('a, Execution_trace.t list) Lwt_result.t
