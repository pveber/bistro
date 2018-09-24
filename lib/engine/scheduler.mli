(* open Bistro *)

type t

val create :
  ?loggers:Logger.t list ->
  ?np:int ->
  ?mem:[`GB of int] ->
  ?use_docker:bool ->
  Db.t -> t

(* val build :
 *   ?loggers:Logger.t list ->
 *   ?np:int ->
 *   ?mem:[`GB of int] ->
 *   ?use_docker:bool ->
 *   Db.t ->
 *   Bistro_base.Workflow.t list ->
 *   t *)

(* val submit :
 *   t -> 'a workflow -> Execution_trace.t Lwt.t
 * 
 * val eval_expr :
 *   t -> 'a Expr.t -> ('a, (string * Execution_trace.t) list) result Lwt.t *)

val start : t -> unit

val join : t -> unit Lwt.t

(* val eval_expr_main :
 *   ?np:int ->
 *   ?mem:[`GB of int] ->
 *   ?loggers:Logger.t list ->
 *   ?use_docker:bool ->
 *   Db.t ->
 *   'a Expr.t ->
 *   ('a, string) result *)
