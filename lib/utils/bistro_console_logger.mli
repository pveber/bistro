open Bistro_engine

type t

val create : unit -> t

val event : t -> Scheduler.time -> Scheduler.event -> unit
