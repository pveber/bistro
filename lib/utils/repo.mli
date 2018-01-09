open Bistro.Std
open Bistro_engine

type item

type t = item list

val ( %> ) : string list -> _ workflow -> item

val item : string list -> _ workflow -> item

val singleton : string -> _ workflow -> t

val add_prefix : string list -> t -> t

val shift : string -> t -> t

val to_term :
  ?precious:Bistro.any_workflow list ->
  outdir:string ->
  t ->
  unit Term.t

val build  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?logger:Scheduler.logger ->
  ?keep_all:bool ->
  ?use_docker:bool ->
  ?precious:Bistro.any_workflow list ->
  ?bistro_dir:string ->
  outdir:string -> t -> unit

val dry_run :
  ?precious:Bistro.any_workflow list ->
  t -> Scheduler.dry_run
