open Bistro_base

val mount_options :
  host_paths:string list ->
  container_paths:string list ->
  string

val image_url : Command.docker_image -> string

val chown_command :
  path:string ->
  uid:int ->
  string
