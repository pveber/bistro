(** A library to build scientific workflows.

    This module introduces a type ['a workflow] that describes a set
   of inter-dependent steps that will eventually generate a value of
   type ['a]. Steps may be either command lines to be executed, or
   OCaml expressions to be evaluated.

    To build shell-based workflows, use the {!module:Shell_dsl}
   module, that provides a set of combinators to write shell scripts
   easily. For instance, the following function shows how to create a
   gzipped file using the output of another workflow:

    {[
      let gzip (x : 'a pworkflow) : 'a gz pworkflow =
        Workflow.shell ~descr:"unix.gzip" [
          cmd "gzip" [ string "-c" ; dep x ; string ">" dest ]
        ]
    ]}
*)


(** {2 Base types} *)

type 'a workflow
(** Representation of a computational pipeline. Constructors are
   provided in the {!Workflow} module. Note that a workflow is just a
   recipe to build some result. Building the workflow won't actually
   generate anything. In order to run the workflow, you have to run it
   using an execution engine like the one provided by [bistro.engine].
   *)

type 'a path
(** Abstract representation of a path in the filesystem. The type
   parameter can be used to provide information on the format of a
   file (this is an instance of phantom-typing). *)

(** Base class for files when typing a {!path} *)
class type regular_file_t = object
  method file_kind : [`regular]
end

(** Base class for directories when typing a {!path} *)
class type directory_t = object
  method file_kind : [`directory]
end

type 'a file = (#regular_file_t as 'a) path workflow
(** Type alias for workflows that produce a regular file *)

type 'a directory = < directory_t ; contents : 'a > path workflow
(** Type alias for workflows that produce a directory *)

(** {2 Building shell-based workflow} *)

type container_image

(** Representation of scripts *)
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

  val deps :
    ?quote:char ->
    sep:string ->
    _ path list workflow ->
    template

  val string_dep : string workflow -> template
  (** [string_dep w] is interpreted as the result of workflow [w] *)

  val int_dep : int workflow -> template
  (** [int_dep w] is interpreted as result of workflow [w] *)

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

(** Command-line construction *)
module Shell_dsl : sig
  type template = Template_dsl.template
  type command

  include module type of Template_dsl with type template := template

  val cmd :
    string ->
    ?stdin:template -> ?stdout:template -> ?stderr:template ->
    template list -> command
  (** Command-line constructor, e.g.
        [cmd "echo" ~stdout:dest [ string "foo" ]]
      will generate a shell command like
        ["echo foo > /some/path"].

      @param env specifies a Docker image where to run the command
      @param stdin adds a ["< /some/path"] token at the end of the command
      @param stdout adds a ["> /some/path"] token at the end of the command
      @param stderr adds a ["2> /some/path"] token at the end of the command *)

  val bash :
    template ->
    command
  (** Run a bash script, best used with [%script {|...|}] *)

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

  (** {4 Useful commands} *)

  val mkdir : template -> command
  val mkdir_p : template -> command
  val cd : template -> command
  val rm_rf : template -> command
  val mv : template -> template -> command

  val docker_image :
    ?tag:string ->
    ?registry:string ->
    account:string ->
    name:string ->
    unit -> container_image
  (** Construct a description of a publicly available docker image *)

  val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
end

(** Workflow constructors *)
module Workflow : sig
  val input :
    ?version:int ->
    string -> 'a path workflow
  (** Workflow constructor from an existing path *)

  val shell :
    ?descr:string ->
    ?mem:int workflow ->
    ?np:int ->
    ?version:int ->
    ?img:container_image list ->
    Shell_dsl.command list -> 'a path workflow
  (** Constructor for a workflow that execute a shell script. Its main
    argument is a list of {!Shell_dsl.cmd} values. Other arguments
    are:
      - {b descr} description of the workflow, used for logging
      - {b mem} required memory
      - {b np} maximum number of cores (could be given less at execution)
      - {b version} version number, used to force the rebuild of a workflow
  *)

  val select :
    _ directory ->
    string list ->
    'a path workflow
  (** Constructs a workflow from a directory workflow, by selecting a
     file in it *)

  val plugin :
    ?descr:string ->
    ?np:int ->
    ?mem:int workflow ->
    ?version:int ->
    (unit -> 'a) workflow ->
    'a workflow

  val path_plugin :
    ?descr:string ->
    ?np:int ->
    ?mem:int workflow ->
    ?version:int ->
    (string -> unit) workflow ->
    'a path workflow

  val pure : id:string -> 'a -> 'a workflow
  (** [pure ~id x] is a workflow that computes the value [x]. [id]
     should be a string identifying [x], like a digest. *)

  val data : 'a -> 'a workflow
  (** Similar to {!pure}, but computes a digest as identifier. Does
     not work with closures or objects. *)

  val int : int -> int workflow
  (** [int i] is [pure_data i] *)

  val string : string -> string workflow
  (** [string s] is [pure_data s] *)

  val app : ('a -> 'b) workflow -> 'a workflow -> 'b workflow
  (** Applicative structure *)

  val both : 'a workflow -> 'b workflow -> ('a * 'b) workflow
  (** Applicative structure, useful for parallel binds *)

  val path : 'a path workflow -> string workflow

  val path_list :
    'a path workflow list -> string list workflow

  val list :
    'a workflow list -> 'a list workflow

  val spawn :
    'a list workflow ->
    f:('a workflow -> 'b workflow) ->
    'b list workflow

  val spawn2 :
    'a list workflow ->
    'b list workflow ->
    f:('a workflow -> 'b workflow -> 'c workflow) ->
    'c list workflow

  val glob :
    ?pattern:string ->
    ?type_selection:[`File | `Directory] ->
    _ directory ->
    'a path list workflow

  val trywith :
    'a workflow ->
    'a workflow ->
    'a workflow

  val ifelse :
    bool workflow ->
    'a workflow ->
    'a workflow ->
    'a workflow
end

(** {2 File formats} *)

class type text = object
  inherit regular_file_t
  method encoding : [`text]
end

class type ['a] sexp_value = object
  inherit regular_file_t
  method ty : 'a
end

class type binary_file = object
  inherit regular_file_t
  method encoding : [`binary]
end

class type pdf = object
  inherit text
  method format : [`pdf]
end

class type html = object
  inherit text
  method format : [`html]
end

class type png = object
  inherit binary_file
  method format : [`png]
end

class type svg = object
  inherit text
  method format : [`svg]
end

class type tsv = object
  inherit text
  method colum_separator : [`tab]
end

class type csv = object
  inherit text
  method colum_separator : [`comma]
end

class type ['a] zip = object
  inherit binary_file
  method format : [`zip]
  method content_format : 'a
end

class type ['a] gz = object
  constraint 'a = #regular_file_t
  inherit binary_file
  method format : [`gz]
  method content_format : 'a
end

class type ['a] bz2 = object
  constraint 'a = #regular_file_t
  inherit binary_file
  method format : [`bz2]
  method content_format : 'a
end

class type ['a] tar = object
  inherit binary_file
  method format : [`tar]
  method content_format : 'a
end

(** Access to internal representation *)
module Private : sig
  val reveal : 'a workflow -> 'a Bistro_internals.Workflow.t
end

(* val file_size : file path workflow -> int workflow
 * val nb_lines : text path workflow -> int workflow
 * val linear_size : float -> file path workflow -> int workflow *)
