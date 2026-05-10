type constant =
  | Pconst_integer of string

and expression = {
  pexp_desc: expression_desc ;
}

and expression_desc =
  | Pexp_constant of constant
  | Pexp_shell_block of string

and structure_item = {
  pstr_desc: structure_item_desc
}

and structure_item_desc =
  | Pstr_value of string * expression

and structure = structure_item list
