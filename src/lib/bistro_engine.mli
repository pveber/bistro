type backend =
  np:int -> mem:int -> timeout:Bistro_workflow.duration ->
  interpreter:Bistro_workflow.interpreter ->
  stdout:string -> stderr:string ->
  string -> [`Ok | `Error]

val local_worker : Bistro_log.t -> backend

val run :
  Bistro_db.t -> Bistro_log.t -> backend ->
  _ Bistro_workflow.t -> unit

val build_repo :
  base:string ->
  ?wipeout:bool->
  Bistro_db.t -> Bistro_log.t -> backend -> Bistro_repo.t -> unit
