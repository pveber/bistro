{
  open Camlp4.PreCast
  open Lexing
  open Bistro_script_parser

  (* Locating antiquotations *)
  let location lexbuf offset =
    let pos = lexeme_start_p lexbuf in
    let pos = { pos with pos_cnum = pos.pos_cnum + offset } in
    Loc.of_lexing_position pos

  let expr lexbuf offset text =
    Syntax.Gram.parse Syntax.expr_eoi (location lexbuf offset) (Stream.of_string text)


}

rule token = parse
| [' ''\t']+ as s
    { SPACE s }

| "\\\n"
    { SPACE "\\\n" }

| '\n'
    { EOL }

| [^ '\n' '%' ' ' '\t' '\n' '\'' '"' ]+ as s
    { STRING s }

| '%' '%'
    { STRING "%" }

| '\''
    { QUOTE '\''}

| '"'
    { QUOTE '"'}

| '`'
    { QUOTE '`'}

| '%' ([^ ':' '%' '@']+ as typ) ':' ([^'%']* as e) '%'
    { ANTIQUOT (typ, expr lexbuf (2 + String.length typ) e) }

| '%' '@'
    { DEST }

| '%' '@' 'T' 'M' 'P'
    { TMP }

| '%' ([^ '%' '@']+ as e) '%'
    { ANTIQUOT ("w", expr lexbuf 1 e) }

| eof
    { EOF }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }










