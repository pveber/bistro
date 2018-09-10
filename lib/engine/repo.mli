open Bistro_base

include Sigs.Repo with type 'a workflow := 'a Workflow.t
                   and type 'a expr := 'a Expr.t
                   and type logger := Logger.t

val to_expr :
  outdir:string ->
  t ->
  unit Expr.t
