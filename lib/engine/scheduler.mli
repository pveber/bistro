type error = [
  | `Msg of string
]

type config = {
  db : Db.t ;
  use_docker : bool ;
}

val eval :
  config ->
  'a Bistro.workflow ->
  'a Lwt.t
