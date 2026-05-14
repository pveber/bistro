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
  | Texp_shell_block of shell_block

and structure_item = {
  tstr_desc: structure_item_desc
}

and structure_item_desc =
  | Tstr_value of string * expression

and structure = structure_item list

and shell_block = shell_cmd list

and shell_cmd = {
  cmd : shell_atom list ;
  std_redir : shell_atom list option
}

and shell_atom =
  | Shell_word of string
  | Shell_antiquot of expression
  | Shell_dest

val type_structure :
  Parsetree.structure ->
  Env.t ->
  structure
