type 'a workflow

type 'a path

class type directory = object
  method file_kind : [`directory]
end

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

  val dep : _ path workflow -> template
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

module Shell_dsl : sig
  type template
  type command
  type docker_image

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

module Workflow : sig
  val cached_value :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    ?version:int ->
    (unit -> 'a) workflow ->
    'a workflow

  val input :
    ?version:int ->
    string -> 'a path workflow

  val cached_path :
    ?descr:string ->
    ?np:int ->
    ?mem:int ->
    ?version:int ->
    (string -> unit) workflow ->
    'a path workflow

  val select :
    #directory path workflow ->
    string list ->
    'a path workflow

  val shell :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    Shell_dsl.command list -> 'a path workflow
  (** Workflow constructor, taking a list of commands in input. Other arguments are:
      - @param descr description of the workflow, used for logging
      - @param mem required memory
      - @param np maximum number of cores (could be given less at execution)
      - @param version version number, used to force the rebuild of a workflow *)

  val pure : id:string -> 'a -> 'a workflow
  val pure_data : 'a -> 'a workflow
  val int : int -> int workflow
  val string : string -> string workflow
  val app : ('a -> 'b) workflow -> 'a workflow -> 'b workflow
  val both : 'a workflow -> 'b workflow -> ('a * 'b) workflow

  val eval_path : 'a path workflow -> string workflow

  val spawn :
    'a list workflow ->
    f:('a workflow -> 'b workflow) ->
    'b list workflow
end


module Private : sig
  val reveal : 'a workflow -> 'a Bistro_internals.Workflow.t
end

(** {5 File formats} *)

class type file = object
  method file_type : [`regular]
end

class type text_file = object
  inherit file
  method encoding : [`text]
end

class type binary_file = object
  inherit file
  method encoding : [`binary]
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
