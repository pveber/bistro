(** A library to build scientific workflows.

    This module introduces a type ['a worfklow] that describe a set of
    inter-dependent actions that will eventually generate a target
    (file or directory). The ['a] type variable represents the format
    of the file or the layout of the directory. Actions may be either
    command lines to be executed, or OCaml expressions to be
    evaluated.

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


(** Helper functions to represent paths as string lists. For absolute
    paths, the first element of the list is ["/"]. *)
module Path : sig
  type t = string list
  [@@deriving sexp]

  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string

  (** [make_relative ~from:dirA dirB] constructs a relative path that
      can be used to go from [dirA] to [dirB]. @raise
      [Invalid_argument] if [dirA] is relative. *)
  val make_relative : ?from:string -> string -> t

  val is_prefix : prefix:t -> t -> bool
  val is_strict_prefix : prefix:t -> t -> bool
end

type id = string

type dep = [
    `Task of id
  | `Select of id * Path.t
  | `Input of Path.t
]

type env = <
  dep : dep -> string ;
  np : int ;
  mem : int ;
  tmp : string ;
  dest : string
>


(** Name and version of an external dependency for a workflow *)
type docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}

module Command : sig
  type 'a t =
    | Docker of docker_image * 'a t
    | Simple_command of 'a token list
    | And_list of 'a t list
    | Or_list of 'a t list
    | Pipe_list of 'a t list

  and 'a token =
    | S of string
    | D of 'a
    | I of 'a
    | F of 'a token list
    | DEST
    | TMP
    | NP
    | MEM
    | EXE

  val map :
    f:('a -> 'b) ->
    'a t ->
    'b t

  val deps : 'a t -> 'a list
end

(** Workflow untyped representation *)
type u = private
  | Input of string * Path.t
  | Select of string * u * Path.t (** invariant: [u] cannot be a [Select] *)
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
  | Exec of dep Command.t
  | Par_exec of {
      dir : u ;
      cmd : u -> u Command.t ;
    }
  | Eval of {
      id : string ;
      f : env -> unit ;
    }

module U : sig
  type t = u
  val to_dep : t -> dep
  val select : t -> Path.t -> t
  val id : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end



(** Describes the (relative) path from a ['a directory workflow]
    target to some ['b workflow] target. This is useful to construct
    new workflows by selecting a file or a subdirectory in the result
    of a directory workflow. *)
type (+'a, +'b) selector = private Selector of Path.t

(** The type representing a set of actions (shell scripts or
    evaluations of OCaml expressions) to build a target of type
    ['a]. The type ['a] is a phantom type, which can be used to
    enforce static invariants. *)
module Workflow : sig
  type +'a t

  val u : _ t -> U.t
  val id : _ t -> string
  val compare : 'a t -> 'a t -> int
  val equal : 'a t -> 'a t -> bool
  val to_dep : _ t -> dep

  val of_fun :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    id:id ->
    deps:U.t list ->
    (env -> unit) ->
    'a t
  (** [of_fun ~id ~f ~deps ()] builds a workflow step by executing [f]. [f]
      is passed an [env] object that can be asked where to find
      dependencies in the cache and where to put the result. [id] is a
      string uniquely identifying [f] and [deps] is the list of
      workflows that will be used during the evaluation of [f].

      Other arguments are:
      - @param descr description of the workflow, used for logging
      - @param mem required memory
      - @param np maximum number of cores (could be given less at execution)
      - @param version version number, used to force the rebuild of a workflow

      This function is not meant to be used directly because for a
      given [f] specifying [id] and [deps] manually is
      error-prone. Use the PPX extension instead. *)

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

  val exe : t
  (** Symbol representing the path of the current executable, as
      specified by [Sys.argv.(0)] *)

  val string : string -> t
  (** A chunk of text *)

  val int : int -> t
  (** Int formatting *)

  val float : float -> t
  (** Float formatting *)

  val path : Path.t -> t
  (** Path formatting *)

  val dep : _ Workflow.t -> t
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val insert : _ Workflow.t -> t

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


type 'a workflow = 'a Workflow.t
type any_workflow = Any_workflow : _ Workflow.t -> any_workflow

(** Conventional type to represent directory targets *)
class type ['a] directory = object
  method kind : [`directory]
  method contents : 'a
end


(** This module provides combinators to define new workflows that
    execute shell commands. *)
module EDSL : sig
  include module type of Template with type t := Template.t
  type command

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

  val ( / ) : _ #directory workflow -> ('a, 'b) selector -> 'b workflow
  (** Constructs a workflow by selecting a dir or file from a
      directory workflow *)

  val map_command :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    _ #directory workflow ->
    (_ workflow -> command) ->
    _ #directory workflow

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

  val internal_cmd :
    string ->
    ?stdin:Template.t -> ?stdout:Template.t -> ?stderr:Template.t ->
    Template.t list ->
    command
  (** Alternative command-line constructor, calling the current
      executable as specified by [Sys.argv.(0)]. More precisely
      [internal_cmd subcmd] calls [Sys.argv.(0)] with subcommand
      [subcmd]. *)

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


(** Standard definitions *)
module Std : sig
  type 'a workflow = 'a Workflow.t
  type nonrec ('a, 'b) selector = ('a, 'b) selector
  type nonrec docker_image = docker_image

  (** Conventional type to represent directory targets *)
  class type ['a] directory = object
    method kind : [`directory]
    method contents : 'a
  end

  (** Conventional type to represent file targets. The object type is to
      represent properties of the file, like the type of encoding (text
      or binary) or the format. *)
  class type file = object
    method kind : [`file]
  end

  class type binary_file = object
    inherit file
    method encoding : [`binary]
  end

  (** Conventional type to represent OCaml values saved with the
      {!module:Marshal} module. *)
  class type ['a] marshalled_value = object
    inherit binary_file
    method format : [`marshalled_value]
    method content_type : 'a
  end

  class type ['a] zip = object
    inherit binary_file
    method format : [`zip]
    method content_format : 'a
  end

  class type ['a] gz = object
    constraint 'a = #file
    inherit binary_file
    method format : [`gz]
    method content_format : 'a
  end

  class type ['a] bz2 = object
    constraint 'a = #file
    inherit binary_file
    method format : [`bz2]
    method content_format : 'a
  end

  class type ['a] tar = object
    inherit binary_file
    method format : [`tar]
    method content_format : 'a
  end

  class type text_file = object
    inherit file
    method encoding : [`text]
  end

  (** Conventional type to represent OCaml values saved as
      S-expressions. *)
  class type ['a] sexp_value = object
    inherit text_file
    method format : [`sexp_value]
    method content_type : 'a
  end

  class type pdf = object
    inherit text_file
    method format : [`pdf]
  end

  class type html = object
    inherit text_file
    method format : [`html]
  end

  class type png = object
    inherit binary_file
    method format : [`png]
  end

  class type svg = object
    inherit text_file
    method format : [`svg]
  end

  class type tsv = object
    inherit text_file
    method colum_separator : [`tab]
  end

  module Unix_tools : sig
    val wget :
      ?descr_url:string ->
      ?no_check_certificate:bool ->
      ?user:string ->
      ?password:string ->
      string -> #file workflow
    val gunzip : 'a gz workflow -> 'a workflow
    val bunzip2 : 'a bz2 workflow -> 'a workflow
    val unzip : 'a zip workflow -> 'a workflow
    val tar_xfz :
      ?strip_components:int ->
      'a tar gz workflow ->
      'a workflow
    val crlf2lf : (#text_file as 'a) workflow -> 'a workflow
  end
end
