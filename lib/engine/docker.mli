open Bistro_internals

val mount_options :
  host_paths:string list ->
  container_paths:string list ->
  string

val image_url : Workflow.Docker_image.t -> string

val chown_command :
  path:string ->
  uid:int ->
  string
