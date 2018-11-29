open Bistro_internals


type t = {
  db : Db.t ;
  using_docker : bool ;
  tmp_dir : string ; (* host all execution *)
  dest : string ;    (* expected path for the target *)
  tmp : string ;     (* temp dir for the process *)
  stdout : string ;
  stderr : string ;
  dep : Workflow.path -> string ;
  file_dump : Workflow.path Template.t -> string ;
  np : int ;
  mem : int ;
  uid : int ;
}

val make :
  db:Db.t ->
  use_docker:bool ->
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
