open Core_kernel
open Bistro_base

type t = {
  db : Db.t ;
  using_docker : bool ;
  tmp_dir : string ; (* host all execution *)
  dest : string ;    (* expected path for the target *)
  tmp : string ;     (* temp dir for the process *)
  stdout : string ;
  stderr : string ;
  file_dump : Workflow.u Template.t -> string ;
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

let container_mount db =
  let open Workflow in
  function
  | Shell _ | Closure _ as u ->
    {
      mount_host_location = Db.cache_dir db ;
      mount_container_location = docker_cache_dir ;
      file_container_location = Filename.concat docker_cache_dir (Workflow.id u)
    }

  | Input { path ; id } ->
    let container_path = Filename.concat docker_cache_dir id in
    {
      mount_host_location = path ;
      mount_container_location = container_path ;
      file_container_location = container_path ;
    }

  | Select { dir = (Shell _ | Closure _) as dir ; sel } ->
    {
      mount_host_location = Db.cache_dir db ;
      mount_container_location = docker_cache_dir ;
      file_container_location =
        List.reduce_exn ~f:Filename.concat [
          docker_cache_dir ;
          Workflow.id dir ;
          Path.to_string sel
        ]
    }
  | Select { dir = Input { path ; id } ; sel } ->
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
  | Select { dir = (Select _) ; sel } -> assert false


let dockerize env = {
  db = env.db ;
  tmp_dir = "/bistro" ;
  using_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  file_dump = (fun toks -> Filename.concat docker_cache_dir (Misc.digest toks)) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}
