open Bistro.Std

type target

val ( %> ) : string list -> _ workflow -> target

val simple :
  ?np:int ->
  ?mem:int ->
  target list -> unit
