(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)

type t
(** An abstract type for databases *)

val init : string -> t
(** [init path] builds a value to represent a database located at path
    [path], which can be absolute or relative. The database is created
    on the file system unless a file/directory exists at the location
    [path]. In that case, the existing file/directory is inspected to
    determine if it looks like a bistro database.

    @raise Invalid_argument if [path] is occupied with something else
    than a bistro database. *)

val path : t -> Bistro_workflow.u -> string
(** Path where a workflow's result is stored. *)


(** {5 Access for build engines} *)

val build_path   : t -> Bistro_workflow.u -> string
(** Returns the path where a workflow is supposed to build its
    result. It should be deleted after the execution of a workflow,
    except if the execution failed. *)

val tmp_path   : t -> Bistro_workflow.u -> string
(** Provides a temporary location that a workflow may use during its
    execution. It should be deleted after the execution of a
    workflow, except if the execution failed. *)

val stdout_path : t -> Bistro_workflow.u -> string
(** Returns a path where to store the stdout of the execution of a
    workflow *)

val stderr_path : t -> Bistro_workflow.u -> string
(** Returns a path where to store the stderr of the execution of a
    workflow *)

val log_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val build_dir : t -> string
val tmp_dir : t -> string

(** {5 Logging} *)
module Log_msg : sig
  type level = [ `debug | `info | `warning | `error ]
  type t = Bistro_workflow.u option * level * Core.Time.t * string

  val make : ?w:Bistro_workflow.u -> level -> ('a, unit, string, t) format4 -> 'a
  val to_string : t -> string
end

val log :
  ?hook:(Log_msg.t -> unit) ->
  t -> ?w:Bistro_workflow.u -> Log_msg.level ->
  (Log_msg.t, unit, string, Log_msg.t) format4 -> unit
