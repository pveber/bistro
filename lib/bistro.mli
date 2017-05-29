(** A library to build scientific workflows.

    This module introduces a type ['a worfklow] that describe a set of
    inter-dependent actions that will eventually generate a target
    (file or directory). The ['a] type represents the format of the
    file or the layout of the directory. Actions may be either command
    lines to be executed, or OCaml expressions to be evaluated.

    To build workflows, use the {!module:EDSL} module, that provide a set of
    combinators to write shell scripts easily. For instance, the
    following function shows how to create a gzipped file using the
    output of another workflow:
    {[
      let gzip (x : 'a workflow) : 'a gz workflow =
        workflow ~descr:"unix.gzip" [
          cmd "gzip" [ string "-c" ; dep x ; string ">" dest ]
        ]
    ]}

    Note that a workflow is just a recipe to build some
    result. Building the workflow won't actually generate anything. In
    order to run the workflow, you have to run it using an execution
    engine like the one provided by [bistro.engine].
 *)
open Core_kernel.Std

type +'a workflow
(** The type representing a set of actions (shell scripts or
    (evaluations of OCaml expressions) to build a target of type
    ['a]. The type ['a] is a phantom type, which can be used to
    enforce static invariants. *)

type 'a directory = [`directory of 'a]
(** Conventional type to represent directory targets *)

(** Conventional type to represent file targets. The object type is to
    represent properties of the file, like the type of encoding (text
    or binary) or the format. *)
class type ['a,'b] file = object
  method format : 'a
  method encoding : [< `text | `binary] as 'b
end

(** Conventional type to represent OCaml values saved with the
    {!module:Marshal} module. *)
class type ['a] value = object
  inherit [ [`value of 'a], [`binary] ] file
end

type any_workflow = Workflow : _ workflow -> any_workflow
(** Encapsulate a workflow target type *)

(** Helper functions to represent paths as string lists. For absolute
    paths, the first element of the list is ["/"]. *)
module Path : sig
  type t = string list
  val of_string : string -> t
  val to_string : t -> string

  (** [make_relative ~from:dirA dirB] constructs a relative path that
      can be used to go from [dirA] to [dirB]. @raise
      [Invalid_argument] if [dirA] is relative. *)
  val make_relative : ?from:string -> string -> t
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
  version : int option ; (** Version number of the wrapper *)
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
  | Expr_pure : { id : string ; value : 'a } -> 'a expression
  | Expr_app : ('a -> 'b) expression * 'a expression -> 'b expression
  | Expr_dest : string expression
  | Expr_tmp : string expression
  | Expr_np : int expression
  | Expr_mem : int expression
  | Expr_dep : _ workflow -> string expression
  | Expr_deps : _ workflow list -> string list expression
  | Expr_valdep : 'a value workflow -> 'a expression
  | Expr_valdeps : 'a value workflow list -> 'a list expression

(** Name and version of an external dependency for a workflow *)
and docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}

(** Describes the (relative) path from a ['a directory workflow]
    target to some ['b workflow] target. This is useful to construct
    new workflows by selecting a file or surdirectory in the result of
    a directory workflow. *)
type (-'a, +'b) selector = private Selector of Path.t

module Workflow : sig
  type 'a t = 'a workflow
  val id : _ t -> string
  val id' : u -> string
  val u : _ t -> u
end

(** Represents a text with special symbols *)
module Template : sig
  type t

  val dest : t
  (** Symbol representing the location where a workflow is expected to
      produce its result *)

  val tmp : t
  (** Symbol representing an existing empty directory that can be used
      as a temporary space for a workflow's execution. *)

  val np : t
  (** Symbol representing the number of cores allocated to the
      workflow *)

  val mem : t
  (** Symbol representing the memory size allocated to the workflow,
      in GB. *)

  val string : string -> t
  (** A chunk of text *)

  val int : int -> t
  (** Int formatting *)

  val float : float -> t
  (** Float formatting *)

  val path : Path.t -> t
  (** Path formatting *)

  val dep : _ workflow -> t
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val quote : ?using:char -> t -> t
  (** [quote ~using:c t] surrounds template [t] with character [c] *)

  val option : ('a -> t) -> 'a option -> t
  (** [option f o] is [f x] if [o = Some x] and [string ""]
      otherwise *)

  val list : ('a -> t) -> ?sep:string -> 'a list -> t
  (** list combinator, optional value of [sep] is [","] *)

  val seq : ?sep:string -> t list -> t
  (** another list combinator, default value for [sep] is [""] *)

  val enum : ('a * string) list -> 'a -> t
  (** combinator for enumerations *)

  val file_dump : t -> t
  (** [file_dump t] can be used when a command needs a configuration
      script: at run-time, it will generate a text using [t], save it
      to a path, deterministically chosen as a function of
      [t]. Finally the template [file_dump t] is interpreted as this
      path. *)
end

(** This module provides combinators to define new workflows that
    execute shell commands. *)
module EDSL : sig
  include module type of Template with type t := Template.t

  val workflow :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    command list -> 'a workflow
  (** Workflow constructor, taking a list of commands in input. Other arguments are:
      - @param descr description of the workflow, used for logging
      - @param mem required memory
      - @param np maximum number of cores (could be given less at execution)
      - @param version version number, used to force the rebuild of a workflow *)

  val input : ?may_change:bool -> string -> 'a workflow
  (** Constructs a workflow from an existing file on the
      filesystem. The argument [may_change] indicates that the file
      may be modified, which is detected by giving the workflow a
      digest of the file as an input. *)

  val selector : Path.t -> ('a, 'b) selector
  (** Selector constructor *)

  val ( / ) : 'a directory workflow -> ('a, 'b) selector -> 'b workflow
  (** Constructs a workflow by selecting a dir or file from a
      directory workflow *)

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:Template.t -> ?stdout:Template.t -> ?stderr:Template.t ->
    Template.t list -> command
  (** Command-line constructor, e.g. [cmd "echo" ~stdout:dest [ string
      "foo" ]] will generate a shell command like ["echo foo >
      /some/path"].
      - @param env specifies a Docker image where to run the command
      - @param stdin adds a ["< /some/path"] token at the end of the command
      - @param stdout adds a ["> /some/path"] token at the end of the command
      - @param stderr adds a ["2> /some/path"] token at the end of the command *)

  val opt : string -> ('a -> Template.t) -> 'a -> Template.t
  (** Command-line option formatting, e.g.: [opt "--output" dep dest]
      will be rendered like ["--output /some/path"] *)

  val opt' : string -> ('a -> Template.t) -> 'a -> Template.t
  (** Same as {!val:opt} but renders options with an equal sign,
      e.g. ["--output=/some/path"] *)

  val flag : ('a -> Template.t) -> 'a -> bool -> Template.t
  (** [flag f x b] renders as [f x] if [b] is true *)

  val or_list : command list -> command
  (** OR-sequence of commands ([ || ]) *)

  val and_list : command list -> command
  (** AND-sequence of commands ([ && ]) *)

  val pipe : command list -> command
  (** Pipe of commands ([ | ]) *)

  val ( // ) : Template.t -> string -> Template.t
  (** Similar to {!val:Filename.concat}, but with other types. *)

  (** {5 Useful commands} *)

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

  (** {5 Docker-related} *)

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> docker_image
  (** Construct a description of a publicly available docker image *)

  val docker : docker_image -> command -> command
  (** [docker cmd] transforms [cmd] so that it can be executed in a
      Docker container. *)

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** Function composition *)
end


(** This module provides an alternative DSL to construct workflows
    whose action is to evaluate an OCaml expression. *)
module EDSL' : sig
  val value :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    'a expression ->
    'a value workflow
  (** [value e] is a workflow that produces an OCaml value by
      evaluating [e]. This value is saved automatically in {!val:dest}
      using functions from the {!module:Marshal} module.
      - @param descr description of the workflow, used for logging
      - @param mem required memory
      - @param np maximum number of cores (could be given less at execution) *)

  val file :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    unit expression ->
    (_, _) #file workflow
  (** Same as {!val:value}, the expression describes a side-effet
      which is supposed to create a file at location {!val:dest}. *)

  val directory :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    unit expression ->
    _ directory workflow
  (** Same as {!val:value}, the expression describes a side-effet
      which is supposed to create a directory at location {!val:dest}. *)

  val id : 'a -> string
  (** Utility function to compute a digest of any (non-functional)
      value to a string *)

  val pure : string -> 'a -> 'a expression
  (** [pure id x] is an expression that evaluate in [x]. [id]
      should uniquely identify [x], meaning that no other call to pure
      should be made with arguments [id] and [y] is [y] is not equal
      to [x]. *)

  val app : ('a -> 'b) expression -> 'a expression -> 'b expression
  (** Function application *)

  val ( $ ) : ('a -> 'b) expression -> 'a expression -> 'b expression
  (** Function application (operator style) *)

  val np : int expression
  (** Expression evaluated to the number of cores attributed to the
      workflow *)

  val dest : string expression
  (** Expression evaluated to the location where the workflow has to
      store its result *)

  val dep : _ workflow -> string expression
  (** Expression evaluated to the location where the workflow has to
      store its result *)

  val valdep : 'a value workflow -> 'a expression
  (** [valdep w] is evaluated to the value produced by the value
      workflow [w] *)

  val deps : _ workflow list -> string list expression
  (** Expression for list of dependencies *)

  val valdeps : _ value workflow list -> 'a list expression
  (** Expression for list of dependencies *)

  val int : int -> int expression
  (** Expression for ints *)

  val string : string -> string expression
  (** Expression for strings *)

  val const : ('a -> string) -> 'a -> 'a expression
  (** Expression for constants, given an id function *)
end

(** Standard definitions *)
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
