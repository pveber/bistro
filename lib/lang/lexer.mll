{
open Parser

let update_loc (lexbuf : Lexing.lexbuf) ~lines ~chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + lines;
    pos_bol = pos.pos_cnum - chars;
  }
}

let newline = ('\013'? '\010')
let blank = [' ' '\009']

let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident_start = lower | upper
let ident_char = ident_start | ['0'-'9' '_']

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*

rule token = parse
  | newline
    {
      update_loc lexbuf ~lines:1 ~chars:0 ;
      token lexbuf
    }
  | blank +
    { token lexbuf }
  | eof
    { EOF }
  | "=" { EQUAL }
  | decimal_literal as lit
    { INT lit }
  | "let"
    { LET }
  | lower ident_char * as s
    { LIDENT s }
