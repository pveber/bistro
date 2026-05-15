type constant =
  | Constant_int of int

type expression = {
  hash : string option ;
  desc : exp_desc ;
}

and exp_desc =
  | Lconst of constant
  | Lvar of string
  | Lshell of expression Shell_ast.t

and t = (string * expression) list

let compile_constant (c : Typedtree.constant) =
  match c with
  | Tconst_int i -> Lconst (Constant_int i)

let rec compile_expression (exp : Typedtree.expression) =
  let desc = match exp.texp_desc with
    | Texp_constant c -> compile_constant c
    | Texp_ident lident -> Lvar lident
    | Texp_shell_block sb -> compile_shell_block sb
  in
  { desc ; hash = None }

and compile_shell_block sb =
  Lshell (List.map compile_shell_cmd sb)

and compile_shell_cmd { Shell_ast.cmd ; std_redir } =
  let cmd = List.map compile_shell_atom cmd in
  let std_redir = Option.map compile_shell_atom std_redir in
  { Shell_ast.cmd ; std_redir }

and compile_shell_atom
  : Typedtree.expression Shell_ast.atom -> expression Shell_ast.atom
  = function
    | Word s -> Word s
    | Antiquot a -> Antiquot (compile_expression a)
    | Dest -> Dest

let compile_str_item (i : Typedtree.structure_item) =
  match i.tstr_desc with
  | Tstr_value (lident, expr) ->
    lident, compile_expression expr

let compile (tree : Typedtree.structure) =
  List.map compile_str_item tree
