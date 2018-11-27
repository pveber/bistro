type error = [
  | `Msg of string
]

val eval :
  'a Bistro.workflow -> 'a Lwt.t
