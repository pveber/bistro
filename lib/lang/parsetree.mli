type constant =
  | Pconst_integer of string

and expression = {
  pexp_desc: expression_desc ;
}

and expression_desc =
  | Pexp_constant of constant
  | Pexp_ident of string
  | Pexp_shell_block of expression Shell_ast.t

and structure_item = {
  pstr_desc: structure_item_desc
}

and structure_item_desc =
  | Pstr_value of string * expression

and structure = structure_item list
