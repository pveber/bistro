open Bistro

type t
val make : np:int -> mem:int -> Db.t -> t
val build : t -> _ Workflow.t -> [ `Ok of string
                                 | `Error of (Workflow.u * string) list] Lwt.t
val build_exn : t -> _ Workflow.t -> string Lwt.t
val build' : t -> Workflow.u -> [ `Ok of string
                                 | `Error of (Workflow.u * string) list] Lwt.t
val build_exn' : t -> Workflow.u -> string Lwt.t
val shutdown : t -> unit Lwt.t
