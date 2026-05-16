type value

type t

val create : string -> t

val eval_program :
  t ->
  Lambda.t ->
  value list Lwt.t
