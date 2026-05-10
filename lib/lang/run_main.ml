module I = Parser.MenhirInterpreter

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
      |> Parser_errors.message
      |> Option.some
    | None -> None
  with Stdlib.Not_found -> None

let fail lexbuf (checkpoint : unit I.checkpoint) =
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
  let init = Parser.Incremental.program lexbuf.lex_curr_p in
  loop lexbuf init

let main ~program_path =
  In_channel.with_open_text program_path (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      match parse_program lexbuf with
      | Ok () -> ()
      | Error (`Parser_error e) -> prerr_endline e.msg
    )

open Cmdliner

let cmd =
  let info = Cmd.info "run" in
  let term =
    let open Term.Syntax in
    let+ program_path = Arg.(
        let doc = "$(docv) is a path to the program to execute" in
        let docv = "PROG" in
        let info = info [] ~doc ~docv in
        required & pos 0 (some string) None info
    ) in
    main ~program_path
  in
  Cmd.v info term
