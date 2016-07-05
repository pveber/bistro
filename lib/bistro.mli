(** A library to build scientific workflows. *)
open Core_kernel.Std

type path = string list

type 'a directory = [`directory of 'a]

(** Name and version of an external dependency for a workflow *)
type docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
with sexp

type 'a workflow

type ('a, 'b) selector = private Selector of path

module Expr : sig
  type t

  val dest : t
  val tmp : t
  val np : t
  val mem : t
  val string : string -> t
  val int : int -> t
  val float : float -> t
  val path : path -> t
  val dep : _ workflow -> t
  val quote : ?using:char -> t -> t
  val option : ('a -> t) -> 'a option -> t
  val list : ('a -> t) -> ?sep:string -> 'a list -> t
  val seq : ?sep:string -> t list -> t
  val enum : ('a * string) list -> 'a -> t
end


module EDSL : sig
  include module type of Expr with type t := Expr.t

  type cmd

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:Expr.t -> ?stdout:Expr.t -> ?stderr:Expr.t ->
    Expr.t list -> cmd

  val script :
    string ->
    ?env:docker_image ->
    ?stdin:Expr.t -> ?stdout:Expr.t -> ?stderr:Expr.t ->
    ?args:Expr.t list ->
    Expr.t -> cmd

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> docker_image

  val workflow :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    cmd list -> 'a workflow

  val input : ?may_change:bool -> string -> 'a workflow

  val selector : path -> ('a, 'b) selector
  val ( / ) : 'a workflow -> ('a, 'b) selector -> 'b workflow

  val ( // ) : Expr.t -> string -> Expr.t
  val opt : string -> ('a -> Expr.t) -> 'a -> Expr.t
  val opt' : string -> ('a -> Expr.t) -> 'a -> Expr.t
  val flag : ('a -> Expr.t) -> 'a -> bool -> Expr.t

  val or_list : cmd list -> cmd
  val and_list : cmd list -> cmd
  val pipe : cmd list -> cmd

  val mkdir : Expr.t -> cmd
  val mkdir_p : Expr.t -> cmd
  val wget : string -> ?dest:Expr.t -> unit -> cmd
  val cd : Expr.t -> cmd
  val rm_rf : Expr.t -> cmd
  val mv : Expr.t -> Expr.t -> cmd

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end


module Task : sig
  type t = private {
    id      : id ;
    descr   : string ;
    deps    : dep list ;
    cmd     : cmd ;
    np      : int ; (** Required number of processors *)
    mem     : int ; (** Required memory in MB *)
    timeout : int option ; (** Maximum allowed running time in hours *)
    version : int option ; (** Version number of the wrapper *)
  }

  and dep = [
      `Task of id
    | `Select of id * path
    | `Input of path
  ]
  and id = string

  and cmd =
    | Simple_command of simple_command
    | Run_script of script
    | And_sequence of cmd list
    | Or_sequence of cmd list
    | Pipe_sequence of cmd list

  and simple_command = {
    tokens : token list ;
    env : docker_image option ;
  }

  and script = {
    interpreter : string ;
    args : token list ;
    text : token list ;
    script_env : docker_image option ;
  }

  and template = token list

  and token =
    | S of string
    | D of dep
    | DEST
    | TMP
    | NP
    | MEM
  with sexp

  val classify_workflow : _ workflow -> dep
  val decompose_workflow : _ workflow -> t String.Map.t
end

module Std : sig
  type nonrec 'a workflow = 'a workflow
  type nonrec ('a, 'b) selector = ('a, 'b) selector

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
