open Core_kernel
open Bistro_base

type docker_image = Command.docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type ('a, 'b) selector = Selector of string list

type 'a workflow = 'a Workflow.t
type 'a expr = 'a Bistro_engine.Expr.t
type shell_command = Workflow.shell_command
let input = Workflow.input
let shell = Workflow.shell
let select dir (Selector path) = Workflow.select dir path
let selector x = Selector x
let ( /> ) = select
let map_workflows expr ~f =
  Bistro_engine.Expr.(list_map expr ~f)

let map2_workflows xs ys ~f =
  let open Bistro_engine.Expr in
  pair xs ys
  |> map ~f:(fun (xs, ys) -> List.zip_exn xs ys)
  |> list_map ~f:(fun (x, y) -> f x y)

let glob = Bistro_engine.Expr.glob

type any_workflow = Any_workflow : _ workflow -> any_workflow

include File_formats

type template = Workflow.template
module Template_dsl = Template_dsl
module Shell_dsl = Shell_dsl

type logger = Bistro_engine.Logger.t
let null_logger () = Bistro_engine.Logger.null
let console_logger () = Bistro_engine.Console_logger.create ()

module Expr = Bistro_engine.Expr

let eval_expr ?np ?mem ?loggers ?use_docker ?(bistro_dir = "_bistro") expr =
  let open Bistro_engine in
  let db = Db.init_exn bistro_dir in
  Scheduler.eval_expr_main ?np ?mem ?loggers db expr

let eval_expr_exn ?np ?mem ?loggers ?use_docker ?bistro_dir expr =
  match eval_expr ?np ?mem ?loggers ?use_docker ?bistro_dir expr with
  | Ok x -> x
  | Error msg -> failwith msg

module Repo = Bistro_engine.Repo

module Private = struct
  let reveal (x : 'a workflow) = (x : 'a Workflow.t)

  module Expr = Expr
  let closure = Workflow.closure
end
