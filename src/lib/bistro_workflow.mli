type path = string

type 'a t = private u
(** A workflow producing an output of type ['a] *)

and u =
| Input of path (** Input file *)
| Rule of rule (** Commands to build a target *)
| Select of u * path (** Access to a file in a directory *)
(** Untyped workflow *)

and rule = {
  script : script ; (** Script to execute in order to build the target *)
  interpreter : interpreter ; (** Interpreter that should be used to execute the script *)
  deps : u list ; (** Workflow that must be run before *)
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory *)
  timeout : duration ; (** Maximum allowed running time *)
}

and duration = [`minute | `hour | `day | `week | `month]

and interpreter = [ `bash | `ocaml | `perl | `python | `R | `sh ]
and script = token list
and token =
  | S : string -> token (** A string fragment *)
  | I : int -> token (** An int fragment *)
  | F : float -> token (** A float fragment *)
  | W : 'a t -> token (** Relative path to a workflow's result *)
  | L : token list -> token (** List of fragments *)
  | D : token (** Relative path where the workflow should produce its output *)
  | TMP : token (** Relative path to a temporary location that can be used by the workflow *)

module Types : sig
  type 'a workflow = 'a t

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type package = [`package] directory

  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file

  class type ['a, 'b, 'c, 'd] tabular = object
    inherit [[`tabular], [`text]] file
    method columns : 'a
    method header : [< `yes | `no] as 'b
    method sep : 'c
    method comment : 'd
  end

  type ('a, 'b, 'c) tsv = ('a, 'b, [`tab], 'c) tabular

end

open Types

(** A combinator module to build simple shell scripts

    Examples:

     begin_
       cmd "htseq"
         opt string "-m" mode
         flag "-x"
         arg string path
     end_
*)

module Shell_script : sig
  type 'a workflow = 'a t
  type t = script
  type 'a cons = (t -> 'a) -> 'a



  val begin_ : 'a cons
  (** Script start *)

  val export_path : t -> package workflow list -> 'a cons
  (** Adds package workflows to the PATH environment variable *)

  val cmd : t -> string -> 'a cons
  (** Starts a command *)

  val string : t -> string -> 'a cons
  (** Adds a string *)

  val int : t -> int -> 'a cons
  (** Adds an int *)

  val dest : t -> 'a cons
  (** Adds a destination token (and a space before it) (see {! type : token}) *)

  val tmp : t -> 'a cons
  (** Adds a temporary location token  (and a space before it) *)

  val workflow : t -> 'a workflow -> 'b cons

  val arg : t -> (t -> 'a -> 'b cons) -> 'a -> 'b cons
  (** Adds an argument (and a space before it) *)

  val arg' : t -> (t -> 'a -> 'b cons) -> 'a option -> 'b cons
  (** Same as [arg] but lifted to option values *)

  val param : t -> (t -> 'a -> 'b cons) -> string -> 'a -> 'b cons

  val param' : t -> (t -> 'a -> 'b cons) -> string -> 'a option -> 'b cons

  val flag : t -> string -> 'a cons

  val flag' : t -> string -> bool -> 'a cons

  val stdout_to : t -> 'a cons

  val end_ : t -> t
end

(** {5 Observers}*)
val digest : u -> string
val deps : u -> u list

val depth_first_traversal : u -> init:'a -> f:(u -> 'a -> 'a) -> 'a

(** {5 Constructors} *)
val input : path -> 'a t
val make :
  ?np:int ->
  ?mem:int ->
  ?timeout:duration ->
  ?interpreter:interpreter ->
  script -> 'a t
val select : _ directory t -> path -> _ t


val depends : 'a t -> on:_ t -> 'a t

(** {5 Utilities} *)

val script_to_string : dest:string -> tmp:string -> (u -> path) -> script -> string
val extension_of_interpreter : interpreter -> string
