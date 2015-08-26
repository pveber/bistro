(** A library to build scientific workflows. *)

type path = string list

type interpreter = [
  | `bash
  | `ocaml
  | `ocamlscript
  | `perl
  | `python
  | `R
  | `sh
]

type 'a workflow
type some_workflow = Workflow : _ workflow -> some_workflow

module T : sig
  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type package = [`package] directory

  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a tgz = ([`tgz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file
  type bash_script = ([`bash_script], [`text]) file

  class type ['a] tabular = object ('a)
    constraint 'a = < columns : 'b ; header : ([< `yes | `no] as 'c) ;
                      sep : 'd ; comment : 'e ; .. >
    inherit [[`tabular], [`text]] file
    method columns : 'b
    method header : 'c
    method sep : 'd
    method comment : 'e
  end

  class type ['a] tsv = object
    inherit [ < sep : [`tab] ; .. > as 'a ] tabular
  end
end

open T

module Script : sig
  type t

  type expr

  val make : interpreter -> expr list -> t

  val dest : expr
  val tmp : expr
  val string : string -> expr
  val int : int -> expr
  val float : float -> expr
  val path : path -> expr
  val dep : _ workflow -> expr
  val option : ('a -> expr) -> 'a option -> expr
  val list : ('a -> expr) -> ?sep:string -> 'a list -> expr
  val seq : ?sep:string -> expr list -> expr
  val enum : ('a * string) list -> 'a -> expr

end

module Shell_script : sig
  include module type of Script with type t = Script.t
  type cmd

  val script : cmd list -> t

  val program :
    ?path:package workflow list ->
    ?pythonpath:package workflow list ->
    string ->
    ?stdin:expr -> ?stdout:expr -> ?stderr:expr ->
    expr list -> cmd

  val bash :
    ?path:package workflow list ->
    bash_script workflow ->
    ?stdin:expr -> ?stdout:expr -> ?stderr:expr ->
    expr list -> cmd

  val ( // ) : expr -> string -> expr
  val opt : string -> ('a -> expr) -> 'a -> expr
  val opt' : string -> ('a -> expr) -> 'a -> expr
  val flag : ('a -> expr) -> 'a -> bool -> expr

  val or_list : cmd list -> cmd
  val and_list : cmd list -> cmd
  val pipe : cmd list -> cmd

  val with_env : (string * expr) list -> cmd -> cmd

  val mkdir : expr -> cmd
  val mkdir_p : expr -> cmd
  val wget : string -> ?dest:expr -> unit -> cmd
  val cd : expr -> cmd
  val rm_rf : expr -> cmd
  val mv : expr -> expr -> cmd
end


module Workflow : sig
  type 'a t = 'a workflow

  val input : ?may_change:bool -> string -> 'a t

  val make :
    ?descr:string ->
    ?interpreter:interpreter ->
    ?mem:int ->
    ?np:int ->
    ?timeout:int ->
    ?version:int ->
    Script.t -> 'a t

  val extract : _ directory t -> path -> 'a t
end

(**
   A database to cache workflow result and execution traces

   It is implemented as a directory in the file system.
*)
module Db : sig

  type t
  (** An abstract type for databases *)

  val init : string -> [ `Ok of t
                       | `Error of [ `Corrupted_dbm
                                   | `Malformed_db of string ] ]
  (** [init path] builds a value to represent a database located at path
      [path], which can be absolute or relative. The database is created
      on the file system unless a file/directory exists at the location
      [path]. In that case, the existing file/directory is inspected to
      determine if it looks like a bistro database.

      Returns an [`Error] if [path] is occupied with something else
      than a bistro database. *)

  val init_exn : string -> t

  val workflow_path : t -> _ workflow -> string
  (** Path where a workflow's result is stored. *)

end

module Engine : sig
  type t
  val make : np:int -> mem:int -> Db.t -> t
  val build : t -> _ workflow -> [ `Ok of string
                                 | `Error of (some_workflow * string) list] Lwt.t
  val build_exn : t -> _ workflow -> string Lwt.t
  val shutdown : t -> unit Lwt.t
end
