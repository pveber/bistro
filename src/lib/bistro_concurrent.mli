type backend =
  np:int -> mem:int ->
  stdout:string -> stderr:string ->
  Lwt_process.command list -> unit Lwt.t

val local_worker : np:int -> mem:int -> backend

val exec : Bistro_db.t -> Bistro_log.t -> backend -> _ Bistro_workflow.t -> unit Lwt.t
val dryrun : Bistro_db.t -> _ Bistro_workflow.t -> unit Lwt.t
