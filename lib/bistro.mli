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

type 'a workflow

include module type of Bistro_base.File_formats

val input : ?may_change:bool -> string -> 'a workflow
(** Constructs a workflow from an existing file on the
    filesystem. The argument [may_change] indicates that the file
    may be modified, which is detected by giving the workflow a
    digest of the file as an input. *)

type shell_command

val shell :
  ?descr:string ->
  ?mem:int ->
  ?np:int ->
  ?version:int ->
  shell_command list -> 'a workflow
(** Workflow constructor, taking a list of commands in input. Other arguments are:
    - @param descr description of the workflow, used for logging
    - @param mem required memory
    - @param np maximum number of cores (could be given less at execution)
    - @param version version number, used to force the rebuild of a workflow *)


(** Describes the (relative) path from a ['a directory workflow]
    target to some ['b workflow] target. This is useful to construct
    new workflows by selecting a file or a subdirectory in the result
    of a directory workflow. *)
type (+'a, +'b) selector = private Selector of string list

val selector : string list -> ('a, 'b) selector
(** Selector constructor *)

val ( /> ) : _ #directory workflow -> ('a, 'b) selector -> 'b workflow
(** Constructs a workflow by selecting a dir or file from a
    directory workflow *)

val select : _ #directory workflow -> ('a, 'b) selector -> 'b workflow
(** Constructs a workflow by selecting a dir or file from a
    directory workflow *)

type any_workflow = Any_workflow : _ workflow -> any_workflow

(** Represents a text with special symbols *)
module Template_dsl : sig
  type template

  val dest : template
  (** Symbol representing the location where a workflow is expected to
      produce its result *)

  val tmp : template
  (** Symbol representing an existing empty directory that can be used
      as a temporary space for a workflow's execution. *)

  val np : template
  (** Symbol representing the number of cores allocated to the
      workflow *)

  val mem : template
  (** Symbol representing the memory size allocated to the workflow,
      in GB. *)

  val string : string -> template
  (** A chunk of text *)

  val int : int -> template
  (** Int formatting *)

  val float : float -> template
  (** Float formatting *)

  val dep : _ workflow -> template
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val quote : ?using:char -> template -> template
  (** [quote ~using:c t] surrounds template [t] with character [c] *)

  val option : ('a -> template) -> 'a option -> template
  (** [option f o] is [f x] if [o = Some x] and [string ""]
      otherwise *)

  val list : ('a -> template) -> ?sep:string -> 'a list -> template
  (** list combinator, optional value of [sep] is [","] *)

  val seq : ?sep:string -> template list -> template
  (** another list combinator, default value for [sep] is [""] *)

  val enum : ('a * string) list -> 'a -> template
  (** combinator for enumerations *)

  val file_dump : template -> template
  (** [file_dump t] can be used when a command needs a configuration
      script: at run-time, it will generate a text using [t], save it
      to a path, deterministically chosen as a function of
      [t]. Finally the template [file_dump t] is interpreted as this
      path. *)
end


(** Name and version of an external dependency for a workflow *)
type docker_image = private {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}

module Shell_dsl : sig
  type template
  type command = shell_command

  include module type of Template_dsl with type template := template

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:template -> ?stdout:template -> ?stderr:template ->
    template list -> command
  (** Command-line constructor, e.g. [cmd "echo" ~stdout:dest [ string
      "foo" ]] will generate a shell command like ["echo foo >
      /some/path"].
      - @param env specifies a Docker image where to run the command
      - @param stdin adds a ["< /some/path"] token at the end of the command
      - @param stdout adds a ["> /some/path"] token at the end of the command
      - @param stderr adds a ["2> /some/path"] token at the end of the command *)

  val opt : string -> ('a -> template) -> 'a -> template
  (** Command-line option formatting, e.g.: [opt "--output" dep dest]
      will be rendered like ["--output /some/path"] *)

  val opt' : string -> ('a -> template) -> 'a -> template
  (** Same as {!val:opt} but renders options with an equal sign,
      e.g. ["--output=/some/path"] *)

  val flag : ('a -> template) -> 'a -> bool -> template
  (** [flag f x b] renders as [f x] if [b] is true *)

  val or_list : command list -> command
  (** OR-sequence of commands ([ || ]) *)

  val and_list : command list -> command
  (** AND-sequence of commands ([ && ]) *)

  val pipe : command list -> command
  (** Pipe of commands ([ | ]) *)

  val ( // ) : template -> string -> template
  (** Similar to {!val:Filename.concat}, but with other types. *)

  (** {5 Useful commands} *)

  val mkdir : template -> command
  val mkdir_p : template -> command
  val cd : template -> command
  val rm_rf : template -> command
  val mv : template -> template -> command

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

module Private : sig
  val reveal : 'a workflow -> 'a Bistro_base.Workflow.t
end
