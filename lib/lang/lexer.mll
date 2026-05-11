{
open Lang_parser

let update_loc (lexbuf : Lexing.lexbuf) ~lines ~chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + lines;
    pos_bol = pos.pos_cnum - chars;
  }

let push_lexeme buf (lexbuf : Lexing.lexbuf) =
  Buffer.add_string buf (Lexing.lexeme lexbuf)

let push stack x =
  stack := x :: !stack

let pop stack =
  stack := (
    match !stack with
    | [] -> []
    | _ :: t -> t
  )
}

let newline = ('\013'? '\010')
let blank = [' ' '\009']

let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident_start = lower | upper
let ident_char = ident_start | ['0'-'9' '_']

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*

let shell_word = (lower | upper | ['0'-'9' '_']) +

rule token stack = parse
  | newline
    {
      update_loc lexbuf ~lines:1 ~chars:0 ;
      token stack lexbuf
    }
  | blank +                             { token stack lexbuf }
  | eof                                 { EOF }
  | "="                                 { EQUAL }
  | decimal_literal as lit              { INT lit }
  | "let"                               { LET }
  | lower ident_char * as s             { LIDENT s }
  | "${"
    {
      push stack `Shell ;
      SHELL_LBRACE
    }
  | "}"
    {
      pop stack ;
      RBRACE
    }

and shell_token stack = parse
  | "'"
    {
      let buf = Buffer.create 256 in
      push_lexeme buf lexbuf ;
      shell_quotation buf lexbuf
    }
  | newline
    {
      update_loc lexbuf ~lines:1 ~chars:0 ;
      shell_token stack lexbuf
    }
  | blank +                             { shell_token stack lexbuf }
  | eof                                 { EOF }
  | "${"
    {
      push stack `ML ;
      SHELL_LBRACE
    }
  | shell_word as w                     { SHELL_WORD w }
  | "}"
    {
      pop stack ;
      RBRACE
    }

and shell_quotation buf = parse
  | "'"
    {
      push_lexeme buf lexbuf ;
      SHELL_WORD (Buffer.contents buf)
    }
  | _
    {
      push_lexeme buf lexbuf ;
      shell_quotation buf lexbuf
    }
