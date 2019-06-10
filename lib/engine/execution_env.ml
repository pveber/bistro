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
  allowed_environments : [`Docker | `Singularity | `Guix] list ;
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


let make ~db  ~allowed_environments ~np ~mem ~id =
  let tmp_dir = Db.tmp db id in
  let file_dump toks =
    Filename.concat tmp_dir (Misc.digest toks)
  in
  {
    db ;
    tmp_dir ;
    allowed_environments ;
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
  allowed_environments = [] ;
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

let allows_docker env = List.mem ~equal:Poly.equal env.allowed_environments `Docker

let singularize env = {
  env with allowed_environments = [] ;
}

let find_docker_image env =
  List.find_map env ~f:(function
      | `Docker_image i -> Some i
      | `Guix_environment _
      | `Singularity_image _ -> None
    )

let find_singularity_image env =
  List.find_map env ~f:(function
      | `Guix_environment _
      | `Docker_image _ -> None
      | `Singularity_image i -> Some i
    )

let find_guix_environment env =
  List.find_map env ~f:(function
      | `Guix_environment e -> Some e
      | `Docker_image _
      | `Singularity_image _ -> None
    )

let rec choose_environment allowed_environments (images : Command.env list) =
  match allowed_environments with
  | [] -> `Plain
  | `Docker :: others -> ( (* docker only accepts docker images *)
      match find_docker_image images with
      | Some i -> `Docker_container i
      | None -> choose_environment others images
    )
  | `Singularity :: others -> (
      match find_singularity_image images with
      | Some i -> `Singularity_container (`Singularity_image i)
      | None ->
        match find_docker_image images with
        | Some i -> `Singularity_container (`Docker_image i)
        | None -> choose_environment others images
    )
  | `Guix :: others -> (
      match find_guix_environment images with
      | Some e -> `Guix e
      | None -> choose_environment others images
    )

let rec images_for_singularity allowed_environments = function
  | Command.Simple_command _ -> []
  | And_list xs
  | Or_list xs
  | Pipe_list xs -> images_for_singularity_aux allowed_environments xs
  | Within_env (img, _) ->
    match choose_environment allowed_environments img with
    | `Plain
    | `Guix _
    | `Docker_container _ -> []
    | `Singularity_container img -> [ img ]
and images_for_singularity_aux allowed_environments xs =
  List.concat_map xs ~f:(images_for_singularity allowed_environments)
