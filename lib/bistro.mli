(** A library to build scientific workflows. *)
open Core_kernel.Std

type +'a workflow

type 'a directory = [`directory of 'a]

class type ['a,'b] file = object
  method format : 'a
  method encoding : [< `text | `binary] as 'b
end

class type ['a] value = object
  inherit [ [`value of 'a], [`binary] ] file
end

type any_workflow = Workflow : _ workflow -> any_workflow

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
  action : action ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  timeout : int option ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
  precious : bool ;
}

and action =
  | Exec of command
  | Eval of some_expression

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

and some_expression =
  | Value     : _ expression    -> some_expression
  | File      : unit expression -> some_expression
  | Directory : unit expression -> some_expression

and _ expression =
  | Expr_primitive : { id : string ; value : 'a } -> 'a expression
  | Expr_app : ('a -> 'b) expression * 'a expression -> 'b expression
  | Expr_dest : string expression
  | Expr_tmp : string expression
  | Expr_np : int expression
  | Expr_mem : int expression
  | Expr_dep : _ workflow -> string expression
  | Expr_deps : _ workflow list -> string list expression
  | Expr_valdep : 'a value workflow -> 'a expression

(** Name and version of an external dependency for a workflow *)
and docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}


type (-'a, +'b) selector = private Selector of Path.t

module Workflow : sig
  type 'a t = 'a workflow
  val id : _ t -> string
  val u : _ t -> u
end


module Template : sig
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
  include module type of Template with type t := Template.t

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
    ?stdin:Template.t -> ?stdout:Template.t -> ?stderr:Template.t ->
    Template.t list -> command

  val opt : string -> ('a -> Template.t) -> 'a -> Template.t
  val opt' : string -> ('a -> Template.t) -> 'a -> Template.t
  val flag : ('a -> Template.t) -> 'a -> bool -> Template.t
  val ( // ) : Template.t -> string -> Template.t

  val docker : docker_image -> command -> command
  val or_list : command list -> command
  val and_list : command list -> command
  val pipe : command list -> command

  val mkdir : Template.t -> command
  val mkdir_p : Template.t -> command
  val wget :
    ?no_check_certificate:bool ->
    ?user:string ->
    ?password:string ->
    ?dest:Template.t ->
    string -> command
  val cd : Template.t -> command
  val rm_rf : Template.t -> command
  val mv : Template.t -> Template.t -> command

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> docker_image

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

module EDSL' : sig
  val value :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    'a expression ->
    'a value workflow

  val file :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    unit expression ->
    (_, _) #file workflow

  val directory :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    unit expression ->
    _ directory workflow

  val id : 'a -> string
  val primitive : string -> 'a -> 'a expression
  val app : ('a -> 'b) expression -> 'a expression -> 'b expression
  val ( $ ) : ('a -> 'b) expression -> 'a expression -> 'b expression
  val np : int expression
  val dest : string expression
  val dep : _ workflow -> string expression
  val valdep : 'a value workflow -> 'a expression
  val deps : _ workflow list -> string list expression
  val int : int -> int expression
  val string : string -> string expression
  val const : ('a -> string) -> 'a -> 'a expression
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
