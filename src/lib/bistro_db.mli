type t

val make : string -> t
val setup : t -> unit
val log_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val tmp_dir : t -> string

val path : t -> Bistro_workflow.u -> string
val cache_path : t -> Bistro_workflow.u -> string
val tmp_path   : t -> Bistro_workflow.u -> string
val stdout_path : t -> Bistro_workflow.u -> string
val stderr_path : t -> Bistro_workflow.u -> string

type 'a logger = [ `debug | `info | `warning | `error ] -> ('a,unit,string,unit) format4 -> 'a

val with_logger :
  t -> Bistro_workflow.u -> f:(_ logger -> 'b) -> 'b
