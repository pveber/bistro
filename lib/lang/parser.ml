module I = Lang_parser.MenhirInterpreter

type error_desc = {
  offset : int ;
  line : int ;
  column : int ;
  msg : string ;
}

let mkerror lexbuf msg =
  let pos = Lexing.lexeme_start_p lexbuf in
  let line = pos.pos_lnum in
  let column = pos.pos_cnum - pos.pos_bol + 1 in
  let offset = pos.pos_cnum in
  `Parser_error { offset ; line ; column ; msg }

let message_of_env env =
  try
    match I.top env with
    | Some (I.Element (s, _, _, _)) ->
      I.number s
      |> Lang_parser_errors.message
      |> Option.some
    | None -> None
  with Stdlib.Not_found -> None

let fail lexbuf (checkpoint : Parsetree.structure I.checkpoint) =
  match checkpoint with
  | I.HandlingError env ->
    let msg =
      match message_of_env env with
      | Some msg -> msg
      | None -> "Syntax error"
    in
    Error (mkerror lexbuf msg)
  | _ -> assert false

let make_lexer () =
  let stack = ref [`ML] in
  fun lexbuf ->
    match !stack with
    | [] -> assert false
    | `ML :: _ -> Lexer.token stack lexbuf
    | `Shell :: _ -> Lexer.shell_token stack lexbuf

let parse_program (lexbuf : Lexing.lexbuf) =
  let lexer = make_lexer () in
  let loop lexbuf result =
    let supplier = I.lexer_lexbuf_to_supplier lexer lexbuf in
    I.loop_handle Result.ok (fail lexbuf) supplier result
  in
  let init = Lang_parser.Incremental.program lexbuf.lex_curr_p in
  loop lexbuf init

let string_of_token tok =
  let open Printf in
  match tok with
  | Lang_parser.EOF -> "EOF"
  | LET -> "LET"
  | EQUAL -> "EQUAL"
  | SHELL_WORD s -> sprintf "SHELL_WORD(%s)" s
  | LIDENT s -> sprintf "LIDENT(%s)" s
  | SHELL_LBRACE -> "SH_LBRACE"
  | RBRACE -> "RBRACE"
  | _ -> "OTHER"

let test_lexer prg =
  let lexbuf = Lexing.from_string prg in
  let lexer = make_lexer () in
  let it = ref 0 in
  let rec loop () =
    incr it ;
    if !it > 10 then ()
    else
      let tok = lexer lexbuf in
      let msg = string_of_token tok in
      print_string msg ;
      if tok = EOF then print_newline ()
      else (print_char ' ' ; loop ())
  in
  loop ()

let%expect_test "shell_word_simple" =
  let prg = {|let a = ${ echo bistro }|} in
  test_lexer prg ;
  [%expect {| LET LIDENT(a) EQUAL SH_LBRACE SHELL_WORD(echo) SHELL_WORD(bistro) RBRACE EOF |}]

let%expect_test "shell_word_quote" =
  let prg = {|let a = ${ echo '}' }|} in
  test_lexer prg ;
  [%expect {| LET LIDENT(a) EQUAL SH_LBRACE SHELL_WORD(echo) SHELL_WORD('}') RBRACE EOF |}]

let%expect_test "shell_word_antiquote" =
  let prg = {|let a = ${ echo ${foo} }|} in
  test_lexer prg ;
  [%expect {| LET LIDENT(a) EQUAL SH_LBRACE SHELL_WORD(echo) SH_LBRACE LIDENT(foo) RBRACE RBRACE EOF |}]
