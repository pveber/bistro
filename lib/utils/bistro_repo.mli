open Bistro.Std
open Bistro_engine

type item

val ( %> ) : string list -> _ workflow -> item

val to_app : outdir:string -> item list -> unit Bistro_app.t

val build  :
  ?np:int ->
  ?mem:int ->
  ?logger:Scheduler.logger ->
  ?dag_dump:string ->
  ?keep_all:bool ->
  outdir:string -> item list -> unit
