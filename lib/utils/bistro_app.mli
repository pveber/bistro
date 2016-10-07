open Bistro.Std
open Bistro_engine

type target
type plan = target list

val ( %> ) : string list -> _ workflow -> target

val local :
  ?use_docker:bool ->
  ?np:int ->
  ?mem:int ->
  outdir:string ->
  plan -> unit
