open Bistro_engine

val null : Scheduler.logger

val tee :
  Scheduler.logger ->
  Scheduler.logger ->
  Scheduler.logger
