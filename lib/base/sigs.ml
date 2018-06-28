module type Template_dsl = sig
  type 'a dep
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

  val dep : _ dep -> template
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

module type Shell_dsl = sig
  type template
  type command
  type docker_image

  include Template_dsl with type template := template

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

  val docker : docker_image -> command -> command
  (** [docker cmd] transforms [cmd] so that it can be executed in a
      Docker container. *)

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> docker_image
  (** Construct a description of a publicly available docker image *)

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

module type DSL = sig
  type +'a workflow

  include module type of File_formats

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

  val input : ?may_change:bool -> string -> 'a workflow
  (** Constructs a workflow from an existing file on the
      filesystem. The argument [may_change] indicates that the file
      may be modified, which is detected by giving the workflow a
      digest of the file as an input. *)

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

  type docker_image
  module Shell_dsl : Shell_dsl with type 'a dep := 'a workflow
                                and type command = shell_command
                                and type docker_image := docker_image

end

module type Term = sig
  type 'a thread
  type 'a workflow
  type logger
  type 'a t
  type 'a path = private Path of string

  val pure : 'a -> 'a t

  val pureW : 'a workflow -> 'a path t

  val app : ('a -> 'b) t -> 'a t -> 'b t

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

  val list : 'a t list -> 'a list t

  val assoc : ('a * 'b t) list -> ('a * 'b) list t

  val create :
    ?np:int ->
    ?mem:[`GB of int] ->
    ?loggers:logger list ->
    ?keep_all:bool ->
    ?use_docker:bool ->
    ?bistro_dir:string ->
    'a t -> ('a, string) result thread

  val run :
    ?np:int ->
    ?mem:[`GB of int] ->
    ?loggers:logger list ->
    ?keep_all:bool ->
    ?use_docker:bool ->
    ?bistro_dir:string ->
    'a t -> 'a
end


module type Repo = sig
  type 'a workflow
  type logger

  type item

  type t = item list

  val ( %> ) : string list -> 'a workflow -> item

  val item : string list -> 'a workflow -> item

  val singleton : string -> 'a workflow -> t

  val add_prefix : string list -> t -> t

  val shift : string -> t -> t

  val build  :
    ?np:int ->
    ?mem:[`GB of int] ->
    ?loggers:logger list ->
    ?keep_all:bool ->
    ?use_docker:bool ->
    ?bistro_dir:string ->
    outdir:string -> t -> unit
end
