open Bistro
open Bistro_engine

val workflow_to_channel :
  ?db:Db.t ->
  out_channel ->
  _ workflow ->
  unit

val workflow_to_file :
  ?db:Db.t ->
  string ->
  _ workflow ->
  unit

val gc_state_to_channel :
  ?condensed:bool ->
  ?db:Db.t ->
  out_channel ->
  Scheduler.Gc.state ->
  unit

val gc_state_to_file :
  ?condensed:bool ->
  ?db:Db.t ->
  string ->
  Scheduler.Gc.state ->
  unit
