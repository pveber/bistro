open Bistro

type t
val make : np:int -> mem:int -> Db.t -> t

(** [build w] runs the execution of the workflow [w] and returns the
    path of the result or an error *)
val build : t -> _ Workflow.t -> [ `Ok of string
                                 | `Error of (Workflow.u * string) list] Lwt.t
val build_exn : t -> _ Workflow.t -> string Lwt.t

(** [build] variants for untyped workflows. *)
val build' : t -> Workflow.u -> [ `Ok of string
                                | `Error of (Workflow.u * string) list] Lwt.t
val build_exn' : t -> Workflow.u -> string Lwt.t
val shutdown : t -> unit Lwt.t
