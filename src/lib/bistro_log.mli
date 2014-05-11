type level = [ `debug | `info | `warning | `error ]

type event = [
| `started_build of Bistro_workflow.u
| `finished_build of Bistro_workflow.u
| `failed_build of Bistro_workflow.u
| `msg of level * string
]

type timestamp = Core.Std.Time.t

module Entry : sig
  type t = event * timestamp
  val to_string : t -> string
end

type t

val make : ?db:Bistro_db.t -> ?hook:(Entry.t -> unit) -> unit -> t

val started_build : t -> Bistro_workflow.u -> unit
val finished_build : t -> Bistro_workflow.u -> unit
val failed_build : t -> Bistro_workflow.u -> unit

val debug : t -> ('a,unit,string,unit) format4 -> 'a
val info : t -> ('a,unit,string,unit) format4 -> 'a
val warning : t -> ('a,unit,string,unit) format4 -> 'a
val error : t -> ('a,unit,string,unit) format4 -> 'a
