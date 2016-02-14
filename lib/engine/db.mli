(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
open Core.Std
open Bistro

type t
(** An abstract type for databases *)

val open_exn : string -> t
(** [open_exn path] opens a database located at path [path], which can
    be absolute or relative. If the path does not exist, the function
    creates a fresh database on the filesystem; if it does, it is
    inspected to see if it looks like a bistro database.

    @raise Failure if [path] is occupied with something else than a
    bistro database. *)

val close : t -> unit

val with_open_exn : string -> (t -> 'a Lwt.t) -> 'a Lwt.t

(** {5 Access for build engines} *)

val build_path : t -> Workflow.step -> string
(** Returns the path where a step workflow is supposed to build its
    result. It should be deleted after the execution of a workflow,
    except if the execution failed. *)

val tmp_path : t -> Workflow.step -> string
(** Provides a temporary location that a workflow may use during its
    execution. It should be deleted after the execution of a
    workflow, except if the execution failed. *)

val stdout_path : t -> Workflow.step -> string
(** Returns a path where to store the stdout of the execution of a
    workflow *)

val stderr_path : t -> Workflow.step -> string
(** Returns a path where to store the stderr of the execution of a
    workflow *)

val cache_path : t -> Workflow.step -> string
(** Path where a step workflow's result is stored. *)

val workflow_path : t -> _ Workflow.t -> string
(** Path where a workflow's result is stored. *)

val workflow_path' : t -> Workflow.u -> string

val requested : t -> Workflow.step -> unit
val built : t -> Workflow.step -> unit


(** {5 Traversal} *)

module Stats : sig
  type t = private {
    workflow : Workflow.step ;
    history : (Time.t * event) list ;
    build_time : float option ;
  }
  and event = Built | Requested

end

val fold : t -> init:'a -> f:('a -> Stats.t-> 'a) -> 'a

(** {5 Reporting } *)

val report : t -> Workflow.u -> string
val output_report : t -> Workflow.u -> out_channel -> unit
