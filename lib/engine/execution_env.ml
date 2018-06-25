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
  dep : Workflow.dep -> string ;
  file_dump : Workflow.dep Template.t -> string ;
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
    dep = (Db.dep_path db) ;
    np ;
    mem ;
    uid = Unix.getuid () ;
  }

let docker_cache_dir = "/bistro/data"

let dep_path_in_docker = function
  | `Cached id -> Filename.concat docker_cache_dir id
  | `Select (`Cached id, sel) ->
    List.reduce_exn ~f:Filename.concat [
      docker_cache_dir ;
      id ;
      Path.to_string sel
    ]

let dockerize env = {
  db = env.db ;
  tmp_dir = "/bistro" ;
  using_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = dep_path_in_docker ;
  file_dump = (fun toks -> Filename.concat docker_cache_dir (Misc.digest toks)) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}
