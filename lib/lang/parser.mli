type error_desc = {
  offset : int ;
  line : int ;
  column : int ;
  msg : string ;
}

val parse_program :
  Lexing.lexbuf ->
  (Parsetree.structure,
   [> `Parser_error of error_desc]) result
