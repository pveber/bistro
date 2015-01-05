(** A library to build scientific workflows.

    When performing computations that last days or weeks, it is
    necessary to introduce "backup" points the program can resume
    from in case some failure happens and stops the execution.
    The {! Bistro} module provides {i cached computations} a.k.a {!
    Bistro.workflow : type}: instead of directly computing a value, we
    build a workflow using a simple OCaml function and other
    workflows. The workflow is evaluated only when needed, and the
    result is cached on disk. If the program fails, workflow
    evaluation uses cached values to avoid unncessary calculations.
*)


(** Abstract representation of a cached computation *)
type 'a workflow

(** A {! Term.t} represents the way a function is wrapped to build a
    workflow. A wrapped function (here called {i primitive}) can be
    applied to a constant argument or some workflow.
*)
module Term : sig
  type 'a t
  val prim :
    string ->
    ?version:int ->
    ?np:int ->
    ?mem:int ->
    'a -> 'a t
  val app : ?n:string -> ('a -> 'b) t -> 'a t -> 'b t
  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
  val arg : ?n:string -> ('a -> 'b t) -> 'a -> ('b -> 'c) t -> 'c t

  val string : string -> string t
  val int : int -> int t
  val bool : bool -> bool t
  val workflow : 'a workflow -> 'a t
  val option : ('a -> 'b t) -> 'a option -> 'b option t
  val list : ('a -> 'b t) -> 'a list -> 'b list t
end

(** A collection of functions and values that are provided to a
    primitive and that can be used to execute shell command, log
    messages in proper locations. *)
type env = <
  sh : string -> unit ; (** Execute a shell command (with {v /bin/sh v}) *)
  shf : 'a. ('a,unit,string,unit) format4 -> 'a ;
  stdout : out_channel ;
  stderr : out_channel ;
  out : 'a. ('a,out_channel,unit) format -> 'a ;
  err : 'a. ('a,out_channel,unit) format -> 'a ;
  with_temp_file : 'a. (string -> 'a) -> 'a ;
  np : int ;
  mem : int ; (** in MB *)
>

(** Workflow constructor *)
val workflow : (env -> 'a) Term.t -> 'a workflow


(** {5 Path workflows}

    A computation that produces a file/directory as a side effect
    instead of a value should be wrapped as a {i path workflow}. In
    that case, the primitive receives the path where to produce its
    output.
*)

(** Phantom-typed path. The type variable is meant to carry
    information on the file/directory format. *)
type 'a path = private Path of string

val path_workflow : (string -> env -> unit) Term.t -> 'a path workflow

val extract : [`directory of 'a] path workflow -> string list -> 'b path workflow

val input : string -> 'a path workflow

(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
module Db : sig

  type t
  (** An abstract type for databases *)

  val init : string -> t
  (** [init path] builds a value to represent a database located at path
      [path], which can be absolute or relative. The database is created
      on the file system unless a file/directory exists at the location
      [path]. In that case, the existing file/directory is inspected to
      determine if it looks like a jinx database.

      @raise Invalid_argument if [path] is occupied with something else
      than a jinx database. *)

  val cache_path : t -> _ workflow -> string
  (** Path where a workflow's result is stored. *)


  (** {5 Access for build engines} *)

  val build_path   : t -> _ workflow -> string
  (** Returns the path where a workflow is supposed to build its
      result. It should be deleted after the execution of a workflow,
      except if the execution failed. *)

  val tmp_path   : t -> _ workflow -> string
  (** Provides a temporary location that a workflow may use during its
      execution. It should be deleted after the execution of a
      workflow, except if the execution failed. *)

  val stdout_path : t -> _ workflow -> string
  (** Returns a path where to store the stdout of the execution of a
      workflow *)

  val stderr_path : t -> _ workflow -> string
  (** Returns a path where to store the stderr of the execution of a
      workflow *)

  val history_path : t -> _ workflow -> string
  (** Returns a path where to the usage history of a workflow is
      stored *)

  val log_dir : t -> string
  val cache_dir : t -> string
  val stdout_dir : t -> string
  val stderr_dir : t -> string
  val build_dir : t -> string
  val tmp_dir : t -> string

  (** {5 History read/write} *)
  val used : t -> _ workflow -> unit
  val created : t -> _ workflow -> unit
  val history : t -> _ workflow -> (Core.Time.t * [`created | `used]) list

  (** {5 Logging} *)
  val log : t -> ('a, unit, string, unit) format4 -> 'a
end

module type Configuration = sig
  val db_path : string
  val np : int
  val mem : int
end

module Engine(C : Configuration) : sig
  val eval : 'a workflow -> 'a Lwt.t
end

