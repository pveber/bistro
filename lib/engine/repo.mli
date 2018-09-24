open Bistro

include Bistro_base.Sigs.Repo with type 'a workflow := 'a workflow
                               and type 'a collection := 'a collection
                               and type logger := Logger.t

val to_expr :
  outdir:string ->
  t ->
  unit Static_scheduler.Expr.t
