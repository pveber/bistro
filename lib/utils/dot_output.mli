open Bistro
open Bistro_engine

val to_channel :
  ?db:Db.t ->
  out_channel ->
  _ workflow ->
  unit

val to_file : string -> _ workflow -> unit
