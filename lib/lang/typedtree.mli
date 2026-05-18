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

val type_structure :
  Parsetree.structure ->
  Env.t ->
  structure
