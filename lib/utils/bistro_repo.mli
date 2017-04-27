open Bistro.Std
open Bistro_engine

type item

type t = item list

val ( %> ) : string list -> _ workflow -> item

val to_app :
  ?precious:Bistro.any_workflow list ->
  outdir:string ->
  t ->
  unit Bistro_app.t

val build  :
  ?np:int ->
  ?mem:int ->
  ?logger:Scheduler.logger ->
  ?keep_all:bool ->
  ?precious:Bistro.any_workflow list ->
  outdir:string -> t -> unit
