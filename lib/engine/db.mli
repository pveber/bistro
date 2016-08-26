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

(** {5 Traversal} *)

module Task_table : sig
  val get : db -> string -> Task.t option
  val save : db -> Task.t -> unit
end

module Stats : sig
  type t = private {
    step_id : string ;
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
    targets : Task.t list ;
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
  val get : db -> Task.t -> string option
  val set : db -> Task.t -> string -> unit
end

(** {5 Reporting } *)

val report : t -> Task.t -> string
val output_report : t -> Task.t -> out_channel -> unit


(** {5 Access for build engines} *)

module Task : sig
  val build_path : t -> Task.t -> string
  (** Returns the path where a step workflow is supposed to build its
      result. It should be deleted after the execution of a workflow,
      except if the execution failed. *)

  val tmp_path : t -> Task.t -> string
  (** Provides a temporary location that a workflow may use during its
      execution. It should be deleted after the execution of a
      workflow, except if the execution failed. *)

  val stdout_path : t -> Task.t -> string
  (** Returns a path where to store the stdout of the execution of a
      workflow *)

  val stderr_path : t -> Task.t -> string
  (** Returns a path where to store the stderr of the execution of a
      workflow *)

  val cache_path : t -> Task.t -> string
  (** Path where a step workflow's result is stored. *)

  val in_cache : t -> Task.t -> bool
  (** Tests if the result of [u]'s execution is in cache *)

  val requested : t -> Task.t -> unit
  val built : t -> Task.t -> unit
end


val register_workflow : t -> _ workflow -> unit
val register_workflows : t -> any_workflow list -> unit
val workflow_path : t -> _ workflow -> string
