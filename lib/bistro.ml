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
type shell_command = Workflow.shell_command
let input = Workflow.input
let shell = Workflow.shell
let select dir (Selector path) = Workflow.select dir path
let selector x = Selector x
let ( /> ) = select

type any_workflow = Any_workflow : _ workflow -> any_workflow

include File_formats

module Template_dsl = Template_dsl
module Shell_dsl = Shell_dsl

type logger = Bistro_engine.Scheduler.logger

module Repo = Bistro_engine.Repo

module Private = struct
  let reveal (x : 'a workflow) = (x : 'a Workflow.t)
end
