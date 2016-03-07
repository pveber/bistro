open Bistro.Std
open Bistro_engine

type target

val ( %> ) : string list -> _ workflow -> target

val local :
  ?np:int ->
  ?mem:int ->
  target list -> unit

val with_backend :
  Scheduler.backend ->
  target list ->
  unit
