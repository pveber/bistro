type t

type value_description = {
  hash : string ;
}

val empty : unit -> t

val add_value :
  t ->
  string ->
  value_description ->
  t

val lookup_value_exn :
  t ->
  string ->
  value_description
