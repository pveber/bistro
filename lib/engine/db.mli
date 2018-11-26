(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)

type t
(** An abstract type for databases *)

type id = string

val init : string -> (t, [`Msg of string]) result
(** [init path] creates a database located at path [path], which can
    be absolute or relative. If the path already exists, its contents
    is inspected to see if it looks like a bistro database; if not, a
    fresh database is created on the filesystem.

    Returns an error message if [path] is occupied with something else
    than a bistro database. *)

val init_exn : string -> t
(** @raise Failure*)

val tmp_dir : t -> string
val cache_dir : t -> string
val stdout_dir : t -> string
val stderr_dir : t -> string
val build_dir : t -> string

val tmp : t -> id -> string
val cache : t -> id -> string
val stdout : t -> id -> string
val stderr : t -> id -> string
val build : t -> id -> string

val fold_cache :
  t ->
  init:'a ->
  f:('a -> string -> 'a) ->
  'a
