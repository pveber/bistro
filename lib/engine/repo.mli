open Bistro

type item

type t = item list

val ( %> ) : string list -> _ workflow -> item

val item : string list -> _ workflow -> item

val singleton : string -> _ workflow -> t

val add_prefix : string list -> t -> t

val shift : string -> t -> t

val to_expr :
  ?precious:Bistro.any_workflow list ->
  outdir:string ->
  t ->
  unit Bistro_base.Workflow.expr

val build  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?logger:Scheduler.logger ->
  ?keep_all:bool ->
  ?use_docker:bool ->
  ?precious:Bistro.any_workflow list ->
  ?bistro_dir:string ->
  outdir:string -> t -> unit
