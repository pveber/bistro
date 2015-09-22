(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
open Bistro

type t
(** An abstract type for databases *)

val init : string -> [ `Ok of t
                     | `Error of [ `Corrupted_dbm
                                 | `Malformed_db of string ] ]
(** [init path] builds a value to represent a database located at path
    [path], which can be absolute or relative. The database is created
    on the file system unless a file/directory exists at the location
    [path]. In that case, the existing file/directory is inspected to
    determine if it looks like a bistro database.

    Returns an [`Error] if [path] is occupied with something else
    than a bistro database. *)

val init_exn : string -> t

val stdout_path : t -> Workflow.step -> string
val stderr_path : t -> Workflow.step -> string
val build_path : t -> Workflow.step -> string
val tmp_path : t -> Workflow.step -> string
val cache_path : t -> Workflow.step -> string

val workflow_path : t -> _ Workflow.t -> string
(** Path where a workflow's result is stored. *)

val workflow_path' : t -> Workflow.u -> string

val requested : t -> Workflow.step -> unit
val built : t -> Workflow.step -> unit
