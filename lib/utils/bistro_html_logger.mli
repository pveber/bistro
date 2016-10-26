open Bistro_engine

type t

val start : string -> Task.config -> t
val event : t -> Scheduler.time -> Scheduler.event -> unit
val stop : t -> unit Lwt.t
