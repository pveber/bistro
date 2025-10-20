open Bistro_internals

type file_dump = File_dump of {
    text : string ;
    path : string ;
  }

type t

val make :
  Execution_env.t ->
  Workflow.container_image list ->
  Execution_env.insert Command.t ->
  t

val text : t -> string
val file_dumps : t -> file_dump list
val run : t -> (int * bool) Lwt.t
val container :
  t ->
  [ `Docker_container of Workflow.Docker_image.t
  | `Singularity_container of Workflow.container_image ] option
