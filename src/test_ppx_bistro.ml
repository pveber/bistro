open Bistro_std

let a = 1

let s = Workflow.make ~interpreter:(`ocaml [])
[%bistro {|

let a = {{ int a }}

|}]

