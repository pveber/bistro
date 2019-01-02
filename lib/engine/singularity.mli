open Bistro_internals

val fetch_image :
  Db.t ->
  Command.container_image ->
  (unit, [> `Singularity_failed_pull of int * string]) Lwt_result.t
