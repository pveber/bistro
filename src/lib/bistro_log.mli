type event = [
| `started_build of Bistro_workflow.u
| `finished_build of Bistro_workflow.u
| `msg of level * string
]
and level = [ `debug | `info | `warning | `error ]
and timestamp = Core.Std.Time.t

type t

val make : unit -> t
val event : t ->  (timestamp * event) React.event

val started : t -> Bistro_workflow.u -> unit
val finished : t -> Bistro_workflow.u -> unit
val debug : t -> ('a,unit,string,unit) format4 -> 'a
val to_strings : t -> string React.event
