open Bistro.Std
open Bistro_engine

type target with sexp
type plan = target list with sexp

val ( %> ) : string list -> _ workflow -> target

val load_plan : string -> plan
val save_plan : string -> plan -> unit

val local :
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
