type error_desc = {
  offset : int ;
  line : int ;
  column : int ;
  msg : string ;
}

val parse_program :
  Lexing.lexbuf ->
  (unit, [> `Parser_error of error_desc]) result
