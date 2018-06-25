open Core_kernel
open Bistro_base

type t = {
  using_docker : bool ;
  tmp_dir : string ; (* host all execution *)
  dest : string ;    (* expected path for the target *)
  tmp : string ;     (* temp dir for the process *)
  stdout : string ;
  stderr : string ;
  dep : Workflow.any -> string ;
  file_dump : Workflow.template -> string ;
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
    tmp_dir ;
    using_docker = use_docker ;
    tmp = Filename.concat tmp_dir "tmp" ;
    dest = Filename.concat tmp_dir "dest" ;
    stdout = Db.stdout db id ;
    stderr = Db.stderr db id ;
    file_dump ;
    dep = (fun (Workflow.Any x) -> Db.path db x) ;
    np ;
    mem ;
    uid = Unix.getuid () ;
  }

let dockerize env = {
  tmp_dir = "/bistro" ;
  using_docker = false ;
  dest = "/bistro/dest" ;
  tmp = "/bistro/tmp" ;
  dep = (fun (Workflow.Any d) -> sprintf "/bistro/data/%s" (Workflow.id d)) ;
  file_dump = (fun toks -> sprintf "/bistro/data/%s" (Misc.digest toks)) ;
  np = env.np ;
  mem = env.mem ;
  stdout = env.stdout ;
  stderr = env.stderr ;
  uid = env.uid ;
}
