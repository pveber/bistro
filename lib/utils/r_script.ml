open Base
open Bistro
open Bistro.Template_dsl

type t = template

type expr = t

type arg = t

let make xs = Template_dsl.(seq ~sep:"\n" xs)

let source s = Template_dsl.string s

let dest = Template_dsl.(quote ~using:'"' dest)

let tmp = Template_dsl.(quote ~using:'"' tmp)

let string s =
  Template_dsl.(quote ~using:'"' (string s))

let int i = Template_dsl.int i

let float f = Template_dsl.float f

let dep w = Template_dsl.(quote ~using:'"' (dep w))

let call_gen fn arg xs  =
  let open Template_dsl in
  seq ~sep:"" [
    string fn ;
    string "(" ;
    list ~sep:"," arg xs ;
    string ")" ;
  ]

let call fn args = call_gen fn Fn.id args

let vector f xs = call_gen "c" f xs

let ints xs = vector Template_dsl.int xs

let string_call_gen fn arg xs =
  List.map xs ~f:arg
  |> String.concat ~sep:","
  |> Printf.sprintf "%s(%s)" fn

let ints_dep w =
  Workflow.(app (pure (string_call_gen "c" Int.to_string) ~id:"r_script.ints") w)
  |> Template_dsl.string_dep

let floats xs = vector Template_dsl.float xs

let floats_dep w =
  Workflow.(app (pure (string_call_gen "c" Float.to_string) ~id:"r_script.ints") w)
  |> Template_dsl.string_dep

let strings xs = vector string xs

let strings_dep w =
  Workflow.(app (pure (string_call_gen "c" Fn.id) ~id:"r_script.ints") w)
  |> Template_dsl.string_dep

let deps xs = vector dep xs

let arg ?l e =
  let open Template_dsl in
  match l with
  | None -> e
  | Some label ->
    seq ~sep:"" [ string label ; string "=" ; e ]

let assign var e =
  let open Template_dsl in
  seq ~sep:" " [ string var ; string "<-" ; e ]

let workflow ?descr ?np ?mem ?env script =
  Workflow.shell ?descr ?np ?mem Shell_dsl.[
    cmd "Rscript" ?env [ file_dump script ] ;
  ]

let workflow' ?descr ?np ?mem ?env exprs =
  workflow ?descr ?np ?mem ?env (make exprs)
