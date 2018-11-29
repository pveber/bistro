open Core_kernel
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


let make ~db  ~use_docker ~np ~mem ~id =
  let tmp_dir = Db.tmp db id in
  let file_dump toks =
    Filename.concat tmp_dir (Misc.digest toks)
  in
  {
    db ;
    tmp_dir ;
    using_docker = use_docker ;
    tmp = Filename.concat tmp_dir "tmp" ;
    dest = Filename.concat tmp_dir "dest" ;
    stdout = Db.stdout db id ;
    stderr = Db.stderr db id ;
    file_dump ;
    dep = (Db.path db) ;
    np ;
    mem ;
    uid = Unix.getuid () ;
  }

let docker_cache_dir = "/bistro/data"

type container_mount = {
  mount_host_location : string ;
  mount_container_location : string ;
  file_container_location : string ;
}

let container_mount
  : Db.t -> Workflow.path -> container_mount
  = fun db path ->
    match path with
    | Cache_id id ->
      {
        mount_host_location = Db.cache_dir db ;
        mount_container_location = docker_cache_dir ;
        file_container_location = Filename.concat docker_cache_dir id
      }

    | FS_path path ->
      let id = Misc.digest path in
      let container_path = Filename.concat docker_cache_dir id in
      {
        mount_host_location = path ;
        mount_container_location = container_path ;
        file_container_location = container_path ;
      }

    | Cd (Cache_id id, sel) ->
      {
        mount_host_location = Db.cache_dir db ;
        mount_container_location = docker_cache_dir ;
        file_container_location =
          List.reduce_exn ~f:Filename.concat [
            docker_cache_dir ;
            id ;
            Path.to_string sel
          ]
      }
    | Cd (FS_path path, sel) ->
      let id = Misc.digest path in
      {
        mount_host_location = path ;
        mount_container_location = Filename.concat docker_cache_dir id ;
        file_container_location =
          List.reduce_exn ~f:Filename.concat [
            docker_cache_dir ;
            id ;
            Path.to_string sel
          ]
      }
  | Cd (Cd _, _) -> assert false


let dockerize env = {
  db = env.db ;
  tmp_dir = "/bistro" ;
  using_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  file_dump = (fun toks -> Filename.concat docker_cache_dir (Misc.digest toks)) ;
  dep = (fun u -> (container_mount env.db u).file_container_location) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}
