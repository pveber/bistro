open Bistro_internals

val fetch_image :
  Command.container_image ->
  string ->
  (unit, [> `Singularity_failed_pull of int * string]) Lwt_result.t
(** [fetch_image img dest] downloads either a Singularity of Docker
   image and saves it at location [dest]. *)
