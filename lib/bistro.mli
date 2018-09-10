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

include Bistro_base.Sigs.DSL

type logger = Bistro_engine.Logger.t
val null_logger : unit -> logger
val console_logger : unit -> logger

module Repo : Bistro_base.Sigs.Repo with type 'a workflow := 'a workflow
                                     and type 'a expr := 'a expr
                                     and type logger := logger

module Expr : sig
  val glob :
    ?pattern:string ->
    _ #directory workflow ->
    _ workflow list expr

  val glob_full :
    ?pattern:string ->
    _ #directory workflow ->
    (string * _ workflow) list expr

  val map : 'a expr -> f:('a -> 'b) -> 'b expr

  module List : sig
    val map : 'a list expr -> f:('a -> 'b) -> 'b list expr
    val spawn : 'a list expr -> f:('a -> 'b expr) -> 'b list expr
  end
end

val eval_expr :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:logger list ->
  ?use_docker:bool ->
  ?bistro_dir:string ->
  'a expr -> ('a, string) result

val eval_expr_exn :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:logger list ->
  ?use_docker:bool ->
  ?bistro_dir:string ->
  'a expr -> 'a

module Private : sig
  open Bistro_base
  val reveal : 'a workflow -> 'a Bistro_base.Workflow.t

  val closure :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    string ->
    Workflow.u list ->
    (Workflow.env -> unit) ->
    'a workflow
end
