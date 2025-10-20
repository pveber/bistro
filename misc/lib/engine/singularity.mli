open Bistro_internals

val fetch_image :
  Workflow.container_image ->
  string ->
  (unit, [> `Singularity_failed_pull of int * string]) Lwt_result.t
