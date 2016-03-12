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

(** Name and version of an external dependency for a workflow *)
type package = {
  pkg_name : string ;
  pkg_version : string ;
}

type package_variable = [ `bin | `inc | `lib | `share ]

val string_of_package_variable : package_variable -> string

module Workflow : sig
  type u =
    | Input of string * path
    | Select of string * u * path
    | Step of step

  and step = {
    id : string ;
    descr : string ;
    deps : u list ;
    script : script ;
    np : int ; (** Required number of processors *)
    mem : int ; (** Required memory in MB *)
    timeout : int option ; (** Maximum allowed running time in hours *)
    version : int option ; (** Version number of the wrapper *)
  }

  and script
  with sexp

  type 'a t = private u
  type ('a, 'b) selector = Selector of path

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

  val select : (_ directory as 'a) t -> ('a, 'b) selector -> 'b t

  val u : _ t -> u

  val to_dot : u -> out_channel -> unit
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

  val selector : path -> ('a, 'b) Workflow.selector
  val ( / ) : 'a Workflow.t -> ('a, 'b) Workflow.selector -> 'b Workflow.t

  val dest : expr
  val tmp : expr
  val np : expr
  val mem : expr
  val string : string -> expr
  val int : int -> expr
  val float : float -> expr
  val path : path -> expr
  val dep : _ Workflow.t -> expr
  val pkgvar : package -> package_variable -> expr
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
    ?path:[`package] Workflow.t list ->
    ?pythonpath:[`package] Workflow.t list ->
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
  val heredoc : dest:expr -> expr -> cmd
end

module Script : sig
  type t = Workflow.script
  val make : interpreter -> EDSL.expr list -> t
  val interpreter : t -> interpreter
  val to_string :
    string_of_workflow:(Workflow.u -> string) ->
    tmp:string ->
    dest:string ->
    pkgvar:(package -> package_variable -> string) ->
    np:int ->
    mem:int ->
    t -> string
end

module Std : sig
  type 'a workflow = 'a Workflow.t
  type ('a, 'b) selector = ('a, 'b) Workflow.selector

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a bz2 = ([`bz2 of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a tar'gz = ([`tar'gz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file
  type bash_script = ([`bash_script], [`text]) file

  type png = ([`png],[`binary]) file
  type svg = ([`png],[`text]) file

  class type ['a] tabular = object ('a)
    constraint 'a = < header : 'b ; sep : 'c ; comment : 'd ; .. >
    inherit [[`tabular], [`text]] file
    method header : 'b
    method sep : 'c
    method comment : 'd
  end

  class type ['a] tsv = object
    inherit [ < sep : [`tab] ; comment : [`sharp] ; .. > as 'a ] tabular
  end

  module Unix_tools : sig
    val wget :
      ?descr_url:string ->
      ?no_check_certificate:bool ->
      string -> (_,_) #file workflow
    val gunzip : 'a gz workflow -> 'a workflow
    val bunzip2 : 'a bz2 workflow -> 'a workflow
    val unzip : 'a zip workflow -> 'a workflow
    val tar_xfz : 'a tar'gz workflow -> 'a workflow
    val crlf2lf : (_,[`text]) file workflow -> (_,[`text]) file workflow
  end
end
