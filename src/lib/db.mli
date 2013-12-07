type t

val make : string -> t
val setup : t -> unit
val log_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val tmp_dir : t -> string

val path : t -> Workflow.u -> string
val cache_path : t -> Workflow.u -> string
val tmp_path   : t -> Workflow.u -> string

type 'a logger = [ `debug | `info | `warning | `error ] -> ('a,unit,string,unit) format4 -> 'a

val with_logger :
  t -> Workflow.u -> f:(_ logger -> 'b) -> 'b
