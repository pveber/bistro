type constant =
  | Pconst_int of int

and expression = {
  pexp_hash: string ;
  pexp_desc: expression_desc ;
}

and expression_desc =
  | Pexp_constant of constant

and structure_item = {
  pstr_desc: structure_item_desc
}

and structure_item_desc =
  | Pstr_value of string * expression
