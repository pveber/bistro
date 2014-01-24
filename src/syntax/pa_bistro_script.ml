open Printf
open Camlp4.PreCast

let script_expander _loc _ s =
  let buf = Lexing.from_string s in
  let ast = Bistro_script_parser.script Bistro_script_lexer.token buf
  in
  let expr_of_antiquot typ e = match typ with
    | `workflow -> <:expr< Bistro_workflow.W $e$>>
    | `pkg_bin -> <:expr< Bistro_workflow.(L [ W $e$ ; S "/bin" ]) >>
    | `int -> <:expr< Bistro_workflow.I $e$>>
    | `string -> <:expr< Bistro_workflow.S $e$>>
    | `float -> <:expr< Bistro_workflow.F $e$>>
    | `PATH -> <:expr< Bistro_workflow.export_PATH_cmd $e$ >>
  in
  let expr_of_optantiquot expr format =
    assert false
  in
  let rec extract_quotation c = function
    | [] -> failwith "bistro_script: quotation not ended"
    | Bistro_script_ast.QUOTE c' :: t when c = c' -> [], t
    | h :: t ->
      let q, rest = extract_quotation c t in
      h :: q, rest
  in
  let command cmd =
    let rec aux inquotation = function
      | [] -> <:expr< [] >>

      | Bistro_script_ast.QUOTE c :: rest ->
	let q, rest = extract_quotation c rest in
	let e = <:expr< (Bistro_workflow.L $aux true q$) >> in
	let f = <:expr< $chr:sprintf "%c" c$ >> in
	let g =
	  Ast.ExApp (_loc,
		     Ast.ExId (_loc,
			       Ast.IdAcc (_loc,
					  Ast.IdUid (_loc, "Bistro_workflow"),
					  Ast.IdUid (_loc, "Q"))),
		     Ast.ExTup (_loc, Ast.ExCom (_loc, e, f)))
	in
	<:expr< [ $g$ :: $aux false rest$ ] >>

      | h :: rest ->
	let h = Bistro_script_ast.(match h with
	  | ANTIQUOT (typ, e) ->
	    expr_of_antiquot typ e
	  | OPTANTIQUOT (expr, format) ->
	    expr_of_optantiquot expr format
	  | S s ->
	    <:expr< Bistro_workflow.S $str:s$ >>
	  | D ->
	    <:expr< Bistro_workflow.D >>
	  | TMP ->
	    <:expr< Bistro_workflow.TMP >>
	  | SPACE s when inquotation ->
	    <:expr< Bistro_workflow.S $str:s$ >>
	  | SPACE _ ->
	    <:expr< Bistro_workflow.S " " >>
	  | QUOTE _ -> assert false
	)
	in
	<:expr< [ $h$ :: $aux inquotation rest$ ] >>
    in
    <:expr< Bistro_workflow.Cmd $aux false cmd$ >>
  in
  List.fold_right
    (fun cmd accu -> <:expr< [ $command cmd$ :: $accu$ ] >>)
    ast
    <:expr< [] >>

let () = Quotation.(add "script" Quotation.DynAst.expr_tag) script_expander


















