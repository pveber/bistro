open Bistro_internals

type 'a path = string

class type directory = object
  method file_kind : [`directory]
end

type 'a workflow = 'a Workflow.t

module Workflow = Workflow

module Private = struct
  let reveal x = x
end
