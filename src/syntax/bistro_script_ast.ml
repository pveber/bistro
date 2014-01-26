open Camlp4.PreCast.Syntax.Ast

type t = item list
and item =
| String of string
| Expr of expr_ty * expr
| Dest
| Tmp
| If of expr * t * t
| Opt of patt * expr * t * t

and expr_ty = [
| `int
| `string
| `float
| `workflow
]

(* let rec remove_prefix_spaces = function *)
(*   | Space _ :: t -> remove_prefix_spaces t *)
(*   | x -> x *)

(* let rec remove_trailing_spaces = function *)
(*   | [] -> [] *)
(*   | h :: t -> *)
(*     match remove_trailing_spaces t, h with *)
(*     | [], Space _ -> [] *)
(*     | _ -> h :: t *)

(* let remove_useless_spaces ast = *)
(*   ast *)
(*   |> List.map remove_prefix_spaces *)
(*   |> List.map remove_trailing_spaces *)

(* let simplify ast = *)
(*   ast *)
(*   |> remove_useless_spaces *)
(*   |> List.filter (( <> ) []) *)
