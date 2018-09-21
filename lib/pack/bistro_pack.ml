include Bistro


type logger = Bistro_engine.Logger.t
let null_logger () = Bistro_engine.Logger.null
let console_logger () = Bistro_engine.Console_logger.create ()

(* let eval_expr ?np ?mem ?loggers ?use_docker:_ (\* FIXME *\) ?(bistro_dir = "_bistro") expr =
 *   let open Bistro_engine in
 *   let db = Db.init_exn bistro_dir in
 *   Scheduler.eval_expr_main ?np ?mem ?loggers db expr
 * 
 * let eval_expr_exn ?np ?mem ?loggers ?use_docker ?bistro_dir expr =
 *   match eval_expr ?np ?mem ?loggers ?use_docker ?bistro_dir expr with
 *   | Ok x -> x
 *   | Error msg -> failwith msg *)

module Repo = Bistro_engine.Repo

module Expr = Bistro_engine.Expr
type 'a expr = 'a Expr.t
