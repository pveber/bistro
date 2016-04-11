(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
open Core.Std
open Rresult
open Bistro

type 'a result = ('a, R.msg) Rresult.result

type t
(** An abstract type for databases *)

type db = t

val init : string -> t result
(** [init path] creates a database located at path [path], which can
    be absolute or relative. If the path already exists, its contents
    is inspected to see if it looks like a bistro database; if not, a
    fresh database is created on the filesystem.

    Returns an error message if [path] is occupied with something else
    than a bistro database. *)

val init_exn : string -> t
(** @raise Failure*)

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

val in_cache : t -> Workflow.u -> bool
(** Tests if the result of [u]'s execution is in cache *)

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

module Stats_table : sig
  val fold : db -> init:'a -> f:('a -> Stats.t-> 'a) -> 'a
end

module Wave : sig
  type t = {
    name : string ;
    description : string ;
    targets : Workflow.u list ;
  }
  with sexp

  val to_string : t -> string
  val of_string : string -> t
end

module Wave_table : sig
  val set : db -> string -> Wave.t -> unit
  val fold : db -> init:'a -> f:('a -> Wave.t-> 'a) -> 'a
end


module Submitted_script_table : sig
  val get : db -> Workflow.step -> string option
  val set : db -> Workflow.step -> string -> unit
end

(** {5 Reporting } *)

val report : t -> Workflow.u -> string
val output_report : t -> Workflow.u -> out_channel -> unit
