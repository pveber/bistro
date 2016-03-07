open Bistro
open Bistro_engine

type task = {
  script : string ;
  script_path : string ;
  pbs_script : Pbs.Script.t ;
}

val make_task :
  workdir:string -> queue:string ->
  np:int ->
  mem:int ->
  ?timeout:int ->
  stdout:string -> (* path where stdout of the job is expected *)
  stderr:string -> (* path where stderr of the job is expected *)
  dest:string ->   (* path where the result of the job is expected *)
  tmp:string ->    (* path that can be used for temp files *)
  workflow_path:(Workflow.u -> string) ->
  script:Workflow.script ->
  task

val make : workdir:string -> queue:string -> Scheduler.backend
