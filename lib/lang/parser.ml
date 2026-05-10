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

let parse_program (lexbuf : Lexing.lexbuf) =
  let loop lexbuf result =
    let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    I.loop_handle Result.ok (fail lexbuf) supplier result
  in
  let init = Lang_parser.Incremental.program lexbuf.lex_curr_p in
  loop lexbuf init

let test_shell_block prg =
  let lexbuf = Lexing.from_string prg in
  for i = 0 to 2 do ignore (Lexer.token lexbuf) done ;
  let output = match Lexer.token lexbuf with
    | SHELL_BLOCK s -> s
    | _ -> "not a shell block"
  in
  print_endline output

let%expect_test "shell_block_simple" =
  test_shell_block {|let a = ${ echo bistro }|} ;
  [%expect {| echo bistro |}]

let%expect_test "shell_block_quote" =
  test_shell_block {|let a = ${ echo '}' }|} ;
  [%expect {| echo '}' |}]
