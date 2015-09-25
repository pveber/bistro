(** A library to build scientific workflows. *)

type path = string list

type interpreter = [
  | `bash
  | `ocaml
  | `ocamlscript
  | `perl
  | `python
  | `R
  | `sh
]

type 'a directory = [`directory of 'a]
type package = [`package] directory

module Workflow : sig
  type u =
    | Input of string * path
    | Extract of string * u * path
    | Step of step

  and step = {
    id : string ;
    descr : string ;
    deps : u list ;
    script : script ;
    np : int ; (** Required number of processors *)
    mem : int ; (** Required memory in MB *)
    timeout : int ; (** Maximum allowed running time in hours *)
    version : int option ; (** Version number of the wrapper *)
  }

  and script
  with sexp

  type 'a t = private u

  val id : _ t -> string
  val id' : u -> string

  val input : ?may_change:bool -> string -> 'a t

  val make :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    script -> 'a t

  val extract : _ directory t -> path -> 'a t

  val u : _ t -> u
end

module EDSL : sig
  type expr

  val workflow :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    ?interpreter:interpreter ->
    expr list -> 'a Workflow.t

  val dest : expr
  val tmp : expr
  val string : string -> expr
  val int : int -> expr
  val float : float -> expr
  val path : path -> expr
  val dep : _ Workflow.t -> expr
  val quote : ?using:char -> expr -> expr
  val option : ('a -> expr) -> 'a option -> expr
  val list : ('a -> expr) -> ?sep:string -> 'a list -> expr
  val seq : ?sep:string -> expr list -> expr
  val enum : ('a * string) list -> 'a -> expr
  val use : Workflow.script -> expr

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end


module EDSL_sh : sig
  include module type of EDSL with type expr = EDSL.expr

  type cmd

  val workflow :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    cmd list -> 'a Workflow.t

  val cmd :
    ?path:package Workflow.t list ->
    ?pythonpath:package Workflow.t list ->
    string ->
    ?stdin:expr -> ?stdout:expr -> ?stderr:expr ->
    expr list -> cmd

  val ( // ) : expr -> string -> expr
  val opt : string -> ('a -> expr) -> 'a -> expr
  val opt' : string -> ('a -> expr) -> 'a -> expr
  val flag : ('a -> expr) -> 'a -> bool -> expr

  val or_list : cmd list -> cmd
  val and_list : cmd list -> cmd
  val pipe : cmd list -> cmd

  val with_env : (string * expr) list -> cmd -> cmd

  val mkdir : expr -> cmd
  val mkdir_p : expr -> cmd
  val wget : string -> ?dest:expr -> unit -> cmd
  val cd : expr -> cmd
  val rm_rf : expr -> cmd
  val mv : expr -> expr -> cmd
end

module Script : sig
  type t = Workflow.script
  val make : interpreter -> EDSL.expr list -> t
  val interpreter : t -> interpreter
  val to_string :
    string_of_workflow:(Workflow.u -> string) ->
    tmp:string ->
    dest:string ->
    t -> string
end

