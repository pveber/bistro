open Base
open Bistro_base

type docker_image = Command.docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type ('a, 'b) selector = Selector of string list

type 'a workflow = Workflow.t
type shell_command = Workflow.shell_command
let input = Workflow.input
let shell = Workflow.shell
let select dir (Selector path) = Workflow.select dir path
let selector x = Selector x
let ( /> ) = select

type any_workflow = Any_workflow : _ workflow -> any_workflow

include File_formats

type template = Workflow.template
module Template_dsl = Template_dsl
module Shell_dsl = Shell_dsl

type 'a collection = Workflow.collection

let glob = Workflow.glob
let collection_map = Workflow.list_map

module Private = struct
  let reveal (x : 'a workflow) = (x : Workflow.t)
  let laever x = x
  let collection (x : 'a collection) = (x : Workflow.collection)

  let plugin = Workflow.plugin
end
