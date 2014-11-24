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

val history_path : t -> Bistro_workflow.u -> string
(** Returns a path where to the usage history of a workflow is
    stored *)

val log_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val build_dir : t -> string
val tmp_dir : t -> string

(** {5 History read/write} *)
val used : t -> Bistro_workflow.u -> unit
val created : t -> Bistro_workflow.u -> unit
val history : t -> Bistro_workflow.u -> (Core.Time.t * [`created | `used]) list

(** {5 Logging} *)
val log : t -> ('a, unit, string, unit) format4 -> 'a
