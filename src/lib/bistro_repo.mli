type path = string list
type item = private Item of Bistro_workflow.u * string * path
type t = private Repo of item list

val item : ?descr:string -> path -> 'a Bistro_workflow.t -> item

val make : item list -> t

val setup : ?wipeout:bool -> Bistro_db.t -> t -> string -> unit
