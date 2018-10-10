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

module Private : sig
  open Bistro_base
  val reveal : 'a workflow -> Workflow.t
  val laever : Workflow.t -> 'a workflow
  val collection : 'a collection -> Workflow.collection

  val plugin :
    ?descr:string ->
    ?mem:int ->
    ?np:int ->
    ?version:int ->
    string ->
    Workflow.dep list ->
    (Workflow.env -> unit) ->
    'a workflow
end
