module type Shell_dsl = sig
  type 'a dep

  type fragment

  val dest : fragment
  (** Symbol representing the location where a workflow is expected to
      produce its result *)

  val tmp : fragment
  (** Symbol representing an existing empty directory that can be used
      as a temporary space for a workflow's execution. *)

  val np : fragment
  (** Symbol representing the number of cores allocated to the
      workflow *)

  val mem : fragment
  (** Symbol representing the memory size allocated to the workflow,
      in GB. *)

  val string : string -> fragment
  (** A chunk of text *)

  val int : int -> fragment
  (** Int formatting *)

  val float : float -> fragment
  (** Float formatting *)

  val dep : _ dep -> fragment
  (** [dep w] is interpreted as the path where to find the result of
      workflow [w] *)

  val quote : ?using:char -> fragment -> fragment
  (** [quote ~using:c t] surrounds template [t] with character [c] *)

  val option : ('a -> fragment) -> 'a option -> fragment
  (** [option f o] is [f x] if [o = Some x] and [string ""]
      otherwise *)

  val list : ('a -> fragment) -> ?sep:string -> 'a list -> fragment
  (** list combinator, optional value of [sep] is [","] *)

  val seq : ?sep:string -> fragment list -> fragment
  (** another list combinator, default value for [sep] is [""] *)

  val enum : ('a * string) list -> 'a -> fragment
  (** combinator for enumerations *)

  val file_dump : fragment -> fragment
  (** [file_dump t] can be used when a command needs a configuration
      script: at run-time, it will generate a text using [t], save it
      to a path, deterministically chosen as a function of
      [t]. Finally the template [file_dump t] is interpreted as this
      path. *)

  type command
  type docker_image

  val cmd :
    string ->
    ?env:docker_image ->
    ?stdin:fragment -> ?stdout:fragment -> ?stderr:fragment ->
    fragment list -> command
  (** Command-line constructor, e.g. [cmd "echo" ~stdout:dest [ string
      "foo" ]] will generate a shell command like ["echo foo >
      /some/path"].
      - @param env specifies a Docker image where to run the command
      - @param stdin adds a ["< /some/path"] token at the end of the command
      - @param stdout adds a ["> /some/path"] token at the end of the command
      - @param stderr adds a ["2> /some/path"] token at the end of the command *)

  val opt : string -> ('a -> fragment) -> 'a -> fragment
  (** Command-line option formatting, e.g.: [opt "--output" dep dest]
      will be rendered like ["--output /some/path"] *)

  val opt' : string -> ('a -> fragment) -> 'a -> fragment
  (** Same as {!val:opt} but renders options with an equal sign,
      e.g. ["--output=/some/path"] *)

  val flag : ('a -> fragment) -> 'a -> bool -> fragment
  (** [flag f x b] renders as [f x] if [b] is true *)

  val or_list : command list -> command
  (** OR-sequence of commands ([ || ]) *)

  val and_list : command list -> command
  (** AND-sequence of commands ([ && ]) *)

  val pipe : command list -> command
  (** Pipe of commands ([ | ]) *)

  val ( // ) : fragment -> string -> fragment
  (** Similar to {!val:Filename.concat}, but with other types. *)

  (** {5 Useful commands} *)

  val mkdir : fragment -> command
  val mkdir_p : fragment -> command
  val cd : fragment -> command
  val rm_rf : fragment -> command
  val mv : fragment -> fragment -> command

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

module type L1_dsl = sig
  type 'a workflow

  module Shell_dsl : Shell_dsl with type 'a dep = 'a workflow

  include module type of File_formats

  val shell :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    Shell_dsl.command list -> 'a workflow
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

  val select :
    _ #directory workflow ->
    string list ->
    'a workflow
    (** Selector constructor *)
end

module type L2_dsl = sig
  include L1_dsl

  class type ['a] collection = object
    method path_kind : [`Directory]
    method element_format : 'a
  end

  val map_dir :
    ?glob:string ->
    'a #collection workflow ->
    f:('a workflow -> 'b workflow) ->
    'b collection workflow
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
  type 'a term
  type logger

  type item

  type t = item list

  val ( %> ) : string list -> 'a workflow -> item

  val item : string list -> 'a workflow -> item

  val singleton : string -> 'a workflow -> t

  val add_prefix : string list -> t -> t

  val shift : string -> t -> t

  val to_term :
    ?precious:'a workflow list ->
    outdir:string ->
    t ->
    unit term

  val build  :
    ?np:int ->
    ?mem:[`GB of int] ->
    ?loggers:logger list ->
    ?keep_all:bool ->
    ?use_docker:bool ->
    ?precious:'a workflow list ->
    ?bistro_dir:string ->
    outdir:string -> t -> unit
end
