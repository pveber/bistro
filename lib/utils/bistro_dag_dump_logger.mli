type t

val start : string -> t

val event : t -> Scheduler.time -> Scheduler.event -> unit

val stop : t -> unit Lwt.t
