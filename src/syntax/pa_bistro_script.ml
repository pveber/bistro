open Printf
open Camlp4.PreCast

let script_expander _loc _ s =
  let buf = Lexing.from_string s in
  let ast = Bistro_script_parser.script Bistro_script_lexer.token buf in
  let rec add_item item accu =
    let open Bistro_script_ast in
    let item_expr = match item with
      | String s -> <:expr< Bistro_workflow.S $str:s$ >>
      | Expr (ty, e) -> (
	match ty with
	| `workflow -> <:expr< Bistro_workflow.W $e$>>
	| `int -> <:expr< Bistro_workflow.I $e$>>
	| `string -> <:expr< Bistro_workflow.S $e$>>
	| `float -> <:expr< Bistro_workflow.F $e$>>
      )
      | Dest ->
 	<:expr< Bistro_workflow.D >>
      | Tmp ->
	<:expr< Bistro_workflow.TMP >>
      | If (cond, then_, else_) ->
	<:expr<
	  Bistro_workflow.L (
	    if $cond$
	    then $expr_of_ast then_$
	    else $expr_of_ast else_$
	  )
	>>
      | Opt (patt, e, some, none) ->
	<:expr<
	  Bistro_workflow.L (
	    match $e$ with
              [ Some $patt$ -> $expr_of_ast some$
	      | None -> $expr_of_ast none$]
	  )
	>>
    in
    <:expr< [ $item_expr$ :: $accu$ ] >>
  and expr_of_ast ast =
    List.fold_right add_item ast <:expr< [] >>
  in
  expr_of_ast ast

let () = Quotation.(add "script" Quotation.DynAst.expr_tag) script_expander


















