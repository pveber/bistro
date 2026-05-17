type path =
  | FS of string
  | Cache of string

type constant =
  | Constant_int of int
  | Constant_path of path

type expression = {
  hash : string option ;
  desc : exp_desc ;
}

and exp_desc =
  | Lconst of constant
  | Lvar of string
  | Lshell of expression Shell_ast.t
  | Llam of string * expression

and t = (string * expression) list

module Exp = struct
  let hash x =
    Digest.to_hex (Digest.string (Marshal.(to_string x [No_sharing])))

  let int i = {
    hash = Some (hash ("int", i)) ;
    desc = Lconst (Constant_int i)
  }

  let path p =
    let id = match p with
      | FS p -> ("FS", p)
      | Cache id -> ("cache", id)
    in
    { hash = Some (hash id) ;
      desc = Lconst (Constant_path p) }

  let hash_shell_atom = function
    | Shell_ast.Word s -> Some (hash ("word", s))
    | Dest -> Some (hash "$@")
    | Antiquot e ->
      Option.map (fun h -> hash ("antiquot", h)) e.hash

  let option_list_all xs =
    let exception Exit in
    let f o acc = match o, acc with
      | None, _ | _, None -> raise Exit
      | Some h, Some t -> Some (h :: t)
    in
    try List.fold_right f xs (Some [])
    with Exit -> None

  let hash_shell_cmd { Shell_ast.cmd ; std_redir } =
    let cmd_hash =
      List.map hash_shell_atom cmd
      |> option_list_all
    and std_redir_hash = Option.map hash_shell_atom std_redir in
    match cmd_hash, std_redir_hash with
    | None, _ | _, Some None -> None
    | Some h_cmd, std_redir_h ->
      Some (hash ("cmd", h_cmd, std_redir_h))

  let hash_shell_cmds cmds =
    List.map hash_shell_cmd cmds
    |> option_list_all
    |> Option.map (fun h -> hash ("shell_cmds", h))

  let shell cmds =
    { hash = hash_shell_cmds cmds ; desc = Lshell cmds }

  let lam var body =
    let hash = Option.map (fun body_h -> hash ("lam", var, body_h)) body.hash in
    { hash ; desc = Llam (var, body) }
end

let compile_constant (c : Typedtree.constant) =
  match c with
  | Tconst_int i -> Exp.int i

let rec compile_expression (exp : Typedtree.expression) =
  match exp.texp_desc with
  | Texp_constant c -> compile_constant c
  | Texp_ident lident -> { desc = Lvar lident ; hash = None }
  | Texp_shell_block sb -> compile_shell_block sb

and compile_shell_block sb =
  Exp.shell (List.map compile_shell_cmd sb)

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
