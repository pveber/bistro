open Bistro.Std
open Bistro_engine

type target with sexp
type plan = target list with sexp

val ( %> ) : string list -> _ workflow -> target

val plan_of_channel : in_channel -> plan
val plan_to_channel : plan -> out_channel -> unit

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
