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

module Expr = struct
  open Bistro_engine.Expr

  let glob_full = glob

  let glob ?pattern dir =
    glob ?pattern dir
    |> map ~f:(List.map ~f:snd)

  let map = map

  module List = struct
    let map xs ~f =
      spawn xs ~f:(fun x -> return (f x))
    let spawn = spawn
  end
end

type any_workflow = Any_workflow : _ workflow -> any_workflow

include File_formats

type template = Workflow.template
module Template_dsl = Template_dsl
module Shell_dsl = Shell_dsl

type logger = Bistro_engine.Logger.t
let null_logger () = Bistro_engine.Logger.null
let console_logger () = Bistro_engine.Console_logger.create ()

let eval_expr ?np ?mem ?loggers ?use_docker:_ (* FIXME *) ?(bistro_dir = "_bistro") expr =
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
