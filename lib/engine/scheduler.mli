type error = [
  | `Msg of string
]

val eval :
  'a Bistro.workflow -> ('a, [> error]) Lwt_result.t
