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
                                     and type logger := logger


module Expr : sig
  type 'a t
  val pure : id:string -> 'a -> 'a t

  val pureW : 'a workflow -> 'a workflow t

  val app : ('a -> 'b) t -> 'a t -> 'b t

  val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

  val list : ('a -> 'b t) -> 'a list -> 'b list t
end

val eval_expr :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:logger list ->
  ?use_docker:bool ->
  ?bistro_dir:string ->
  'a Expr.t -> ('a, string) result

module Private : sig
  open Bistro_base
  val reveal : 'a workflow -> 'a Bistro_base.Workflow.t

  module Expr : sig
    type 'a t
    val pure : id:string -> 'a -> 'a t

    val pureW : 'a workflow -> 'a workflow t

    val app : ('a -> 'b) t -> 'a t -> 'b t

    val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

    val list : ('a -> 'b t) -> 'a list -> 'b list t

    val dep : 'a workflow t -> string t
    val deps : 'a workflow list t -> string list t
  end

  val closure :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    (Workflow.env -> unit) Expr.t ->
    'a workflow
end
