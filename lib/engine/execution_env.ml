open Core_kernel
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


let make ~db  ~allowed_containers ~np ~mem ~id =
  let tmp_dir = Db.tmp db id in
  let file_dump toks =
    Filename.concat tmp_dir (Misc.digest toks)
  in
  {
    db ;
    tmp_dir ;
    allowed_containers ;
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

let container_cache_dir = "/bistro/data"

(* This type is used to allow a form of compression, by noticing some
   mounted files are in the same directory, and mount that directory
   only once. This is particularly useful for /bistro/data *)
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
        mount_container_location = container_cache_dir ;
        file_container_location = Filename.concat container_cache_dir id
      }

    | FS_path path ->
      let id = Misc.digest path in
      let container_path = Filename.concat container_cache_dir id in
      {
        mount_host_location = path ;
        mount_container_location = container_path ;
        file_container_location = container_path ;
      }

    | Cd (Cache_id id, sel) ->
      {
        mount_host_location = Db.cache_dir db ;
        mount_container_location = container_cache_dir ;
        file_container_location =
          List.reduce_exn ~f:Filename.concat [
            container_cache_dir ;
            id ;
            Path.to_string sel
          ]
      }
    | Cd (FS_path path, sel) ->
      let id = Misc.digest path in
      {
        mount_host_location = path ;
        mount_container_location = Filename.concat container_cache_dir id ;
        file_container_location =
          List.reduce_exn ~f:Filename.concat [
            container_cache_dir ;
            id ;
            Path.to_string sel
          ]
      }
  | Cd (Cd _, _) -> assert false


let dockerize env = {
  db = env.db ;
  tmp_dir = "/bistro/build" ;
  allowed_containers = [] ;
  dest = "/bistro/build/dest" ;
  tmp = "/bistro/build/tmp" ;
  file_dump = (fun toks -> Filename.concat container_cache_dir (Misc.digest toks)) ;
  dep = (fun u -> (container_mount env.db u).file_container_location) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}

let allows_docker env = List.mem ~equal:Poly.equal env.allowed_containers `Docker

let singularize env = {
  env with allowed_containers = [] ;
           tmp_dir = "/bistro/build" ;
           dest = "/bistro/build/dest" ;
           tmp = "/bistro/build/tmp" ;
           file_dump = (fun toks -> Filename.concat container_cache_dir (Misc.digest toks)) ;
           dep = (fun u -> (container_mount env.db u).file_container_location) ;
}

let find_docker_image env =
  List.find_map env ~f:Workflow.(function
      | Docker_image i -> Some i
      | Singularity_image _ -> None
    )

let find_singularity_image env =
  List.find_map env ~f:Workflow.(function
      | Docker_image _ -> None
      | Singularity_image i -> Some i
    )

let rec choose_container allowed_containers images =
  match allowed_containers with
  | [] -> None
  | `Docker :: others -> ( (* docker only accepts docker images *)
      match find_docker_image images with
      | Some i -> Some (`Docker_container i)
      | None -> choose_container others images
    )
  | `Singularity :: others -> (
      match find_singularity_image images with
      | Some i -> Some (`Singularity_container (Workflow.Singularity_image i))
      | None ->
        match find_docker_image images with
        | Some i -> Some (`Singularity_container (Workflow.Docker_image i))
        | None -> choose_container others images
    )
