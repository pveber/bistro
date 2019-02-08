open Bistro_internals

type insert =
  | Path of Workflow.path
  | Path_list of {
      elts : Workflow.path list ;
      sep : string ;
      quote : char option ;
    }
  | String of string

type t = {
  db : Db.t ;
  allowed_containers : [`Docker | `Singularity] list ;
  tmp_dir : string ; (* host all execution *)
  dest : string ;    (* expected path for the target *)
  tmp : string ;     (* temp dir for the process *)
  stdout : string ;
  stderr : string ;
  dep : Workflow.path -> string ;
  file_dump : insert Template.t -> string ;
  np : int ;
  mem : int ;
  uid : int ;
}

val make :
  db:Db.t ->
  allowed_containers:[`Docker | `Singularity] list ->
  np:int ->
  mem:int ->
  id:string ->
  t

type container_mount = {
  mount_host_location : string ;
  mount_container_location : string ;
  file_container_location : string ;
}

val container_mount : Db.t -> Workflow.path -> container_mount
val dockerize : t -> t
val docker_cache_dir : string
val allows_docker : t -> bool
val singularize : t -> t

val choose_container :
  [`Docker | `Singularity] list ->
  Command.container_image list ->
  [ `Plain
  | `Docker_container of Command.Docker_image.t
  | `Singularity_container of Command.container_image ]

val images_for_singularity :
  [`Docker | `Singularity] list ->
  _ Command.t ->
  Command.container_image list
