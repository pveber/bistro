(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)

module type Thread = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val mkdir_p : string -> unit t
  (** Equivalent of [mkdir -p] *)

  val echo : path:string -> string -> unit t
  (** [echo ~path msg] should append the string [msg] to the file at
      location [path], creating it if it does not exist.

      @raise Invalid_argument if no file can be created at path
      [path] *)
end

module Blocking_thread : Thread with type 'a t = 'a

module Make(T : Thread) : sig

  type t
  (** An abstract type for databases *)

  val make : string -> t
  (** [make path] builds a value to represent a database located at path
      [path], which can be absolute or relative. The database is not
      created/initialized on the file system after a call to [make] *)

  val init : t -> [`Ok | `File_exists] T.t
  (** [init db] initializes the database [db]. It returns [`Ok] if the
      operation succeeds and [`File_exists] if the path used to create
      [db] already exists *)

  val path : t -> Bistro_workflow.u -> string
  (** Path where a workflow's result is stored. *)


  (** {5 Access for build engines} *)

  val build_path   : t -> Bistro_workflow.u -> string
  (** Returns the path where a workflow is supposed to build its
      result. It should be deleted after the execution of a workflow,
      except if the execution failed. *)

  val tmp_path   : t -> Bistro_workflow.u -> string
  (** Provides a temporary location that a workflow may use during its
      execution. It should be deleted after the execution of a workflow,
      except if the execution failed. *)

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
    ?hook:(Log_msg.t -> unit T.t) ->
    t -> ?w:Bistro_workflow.u -> Log_msg.level ->
    (Log_msg.t, unit, string, Log_msg.t) format4 -> unit T.t
end

include module type of Make(Blocking_thread)
