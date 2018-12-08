open Bistro
open Bistro_engine

val to_channel :
  ?db:Db.t ->
  ?gc_state:Logger.gc_state ->
  out_channel ->
  _ workflow ->
  unit

val to_file :
  ?db:Db.t ->
  ?gc_state:Logger.gc_state ->
  string ->
  _ workflow ->
  unit
