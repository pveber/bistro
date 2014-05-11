type backend =
  np:int -> mem:int -> timeout:Bistro_workflow.duration ->
  interpreter:Bistro_workflow.interpreter ->
  stdout:string -> stderr:string ->
  string -> [`Ok | `Error] Lwt.t

val local_worker : np:int -> mem:int -> Bistro_log.t -> backend

val run : Bistro_db.t -> Bistro_log.t -> backend -> _ Bistro_workflow.t -> unit Lwt.t
val dryrun : Bistro_db.t -> _ Bistro_workflow.t -> unit Lwt.t

val build_repo :
  base:string ->
  ?wipeout:bool->
  Bistro_db.t -> Bistro_log.t -> backend -> Bistro_repo.t -> unit Lwt.t
