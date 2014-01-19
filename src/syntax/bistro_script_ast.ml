type t = cmd list
and cmd = token list
and token =
| S of string
| SPACE of string
| ANTIQUOT of antiquotation_typ * Camlp4.PreCast.Syntax.Ast.expr
| QUOTE of char
| D
| TMP
and antiquotation_typ = [
| `int
| `string
| `float
| `workflow
| `pkg_bin
| `PATH
]

let rec remove_prefix_spaces = function
  | SPACE _ :: t -> remove_prefix_spaces t
  | x -> x

let rec remove_trailing_spaces = function
  | [] -> []
  | h :: t ->
    match remove_trailing_spaces t, h with
    | [], SPACE _ -> []
    | _ -> h :: t

let remove_useless_spaces ast =
  ast
  |> List.map remove_prefix_spaces
  |> List.map remove_trailing_spaces

let simplify ast =
  ast
  |> remove_useless_spaces
  |> List.filter (( <> ) [])
