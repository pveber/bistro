open Bistro.Std
open Bistro_engine

type target
type plan = target list

val ( %> ) : string list -> _ workflow -> target

val local :
  ?use_docker:bool ->
  ?np:int ->
  ?mem:int ->
  ?tmpdir:string ->
  outdir:string ->
  plan -> unit

val with_backend :
  Scheduler.backend ->
  outdir:string ->
  plan ->
  unit
