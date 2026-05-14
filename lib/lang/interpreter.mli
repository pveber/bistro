type value

type t

val create : string -> t

val eval_structure :
  t ->
  Typedtree.structure ->
  value list Lwt.t
