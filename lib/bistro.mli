(** A library to build scientific workflows. *)
open Core_kernel.Std

type +'a workflow

module Path : sig
  type t = string list
  val of_string : string -> t
  val make_relative : ?from:string -> string -> t
  val to_string : t -> string
end

(** Workflow representation *)
type u = private
  | Input of string * Path.t
  | Select of string * u * Path.t
  | Step of step

and step = private {
  id : string ;
  descr : string ;
  deps : u list ;
  cmd : command ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int option ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
  precious : bool ;
}

and command =
  | Docker of docker_image * command
  | Simple_command of token list
  | And_list of command list
  | Or_list of command list
  | Pipe_list of command list

and token =
  | S of string
  | D of u
  | F of token list
  | DEST
  | TMP
  | NP
  | MEM

(** Name and version of an external dependency for a workflow *)
and docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}

type 'a directory = [`directory of 'a]

type any_workflow = Workflow : _ workflow -> any_workflow

type (-'a, +'b) selector = private Selector of Path.t

module Workflow : sig
  type 'a t = 'a workflow
  val id : _ t -> string
  val u : _ t -> u
end


module Expr : sig
  type t

  val dest : t
  val tmp : t
  val np : t
  val mem : t
  val string : string -> t
  val int : int -> t
  val float : float -> t
  val path : Path.t -> t
  val dep : _ workflow -> t
  val quote : ?using:char -> t -> t
  val option : ('a -> t) -> 'a option -> t
  val list : ('a -> t) -> ?sep:string -> 'a list -> t
  val seq : ?sep:string -> t list -> t
  val enum : ('a * string) list -> 'a -> t

  val file_dump : t -> t
end


module EDSL : sig
  include module type of Expr with type t := Expr.t

  val workflow :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    command list -> 'a workflow

  val precious : 'a workflow -> 'a workflow

  val input : ?may_change:bool -> string -> 'a workflow

  val selector : Path.t -> ('a, 'b) selector

  val ( / ) : 'a directory workflow -> ('a, 'b) selector -> 'b workflow

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:Expr.t -> ?stdout:Expr.t -> ?stderr:Expr.t ->
    Expr.t list -> command

  val opt : string -> ('a -> Expr.t) -> 'a -> Expr.t
  val opt' : string -> ('a -> Expr.t) -> 'a -> Expr.t
  val flag : ('a -> Expr.t) -> 'a -> bool -> Expr.t
  val ( // ) : Expr.t -> string -> Expr.t

  val docker : docker_image -> command -> command
  val or_list : command list -> command
  val and_list : command list -> command
  val pipe : command list -> command

  val mkdir : Expr.t -> command
  val mkdir_p : Expr.t -> command
  val wget :
    ?no_check_certificate:bool ->
    ?user:string ->
    ?password:string ->
    ?dest:Expr.t ->
    string -> command
  val cd : Expr.t -> command
  val rm_rf : Expr.t -> command
  val mv : Expr.t -> Expr.t -> command

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> docker_image

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
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
      ?user:string ->
      ?password:string ->
      string -> (_,_) #file workflow
    val gunzip : 'a gz workflow -> 'a workflow
    val bunzip2 : 'a bz2 workflow -> 'a workflow
    val unzip : 'a zip workflow -> 'a workflow
    val tar_xfz : 'a tar'gz workflow -> 'a workflow
    val crlf2lf : (_,[`text]) file workflow -> (_,[`text]) file workflow
  end
end


