type t

val create : string -> t

val eval_program :
  t ->
  Lambda.t ->
  Lambda.expression list Lwt.t
