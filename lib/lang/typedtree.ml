type constant =
  | Tconst_int of int

and expression = {
  texp_desc: expression_desc ;
  texp_hash: string ;
  texp_env: Env.t ;
}

and expression_desc =
  | Texp_constant of constant
  | Texp_ident of string
  | Texp_shell_block of expression Shell_ast.t

and structure_item = {
  tstr_desc: structure_item_desc
}

and structure_item_desc =
  | Tstr_value of string * expression

and structure = {
  tmod_inputs : structure_item list ;
  tmod_defs : structure_item list ;
}

let hash x =
  Digest.to_hex (Digest.string (Marshal.(to_string x [No_sharing])))

let eval_constant = function
  | Parsetree.Pconst_integer s -> Tconst_int (int_of_string s)

let hash_shell_atom = function
  | Shell_ast.Word s -> (s, "")
  | Dest -> ("$@", "")
  | Antiquot e ->
    (e.texp_hash, "antiquot")

let hash_shell_cmd { Shell_ast.cmd ; std_redir } =
  hash (
    List.map hash_shell_atom cmd,
    Option.map hash_shell_atom std_redir
  )

let hash_shell_cmds cmds =
  hash ("shell_cmds" :: List.map hash_shell_cmd cmds)

let rec type_expression env (exp : Parsetree.expression) =
  match exp.pexp_desc with
  | Pexp_constant c ->
    let c = eval_constant c in
    {
      texp_desc = Texp_constant c ;
      texp_env = env ;
      texp_hash = hash ("const", c)
    }
  | Pexp_ident lident ->
    let descr = Env.lookup_value_exn env lident in
    {
      texp_desc = Texp_ident lident ;
      texp_env = env ;
      texp_hash = descr.hash ;
    }
  | Pexp_shell_block cmds ->
    let cmds = List.map (type_shell_cmd env) cmds in
    {
      texp_desc = Texp_shell_block cmds ;
      texp_env = env ;
      texp_hash = hash_shell_cmds cmds ;
    }

and type_shell_cmd env { Shell_ast.cmd ; std_redir } =
  let type_atom : Parsetree.expression Shell_ast.atom -> expression Shell_ast.atom = function
    | Word s -> Word s
    | Antiquot e -> Antiquot (type_expression env e)
    | Dest -> Dest
  in
  let cmd = List.map type_atom cmd in
  let std_redir = Option.map type_atom std_redir in
  { cmd ; std_redir }

let type_str_item (acc, env) (item : Parsetree.structure_item) =
  match item.pstr_desc with
  | Pstr_value (lident, exp) ->
    let texp = type_expression env exp in
    let item = { tstr_desc = Tstr_value (lident, texp) } in
    let desc = { Env.hash = texp.texp_hash } in
    let env = Env.add_value env lident desc in
    (item :: acc, env)

let type_structure (past : Parsetree.structure) env =
  let fold_defs env = List.fold_left type_str_item ([], env) in
  let input_items, env = match past.pmod_inputs with
    | None -> [], env
    | Some str_items -> fold_defs env str_items
  in
  let def_items, env = fold_defs env past.pmod_defs in
  {
    tmod_inputs = List.rev input_items ;
    tmod_defs = List.rev def_items ;
  }
