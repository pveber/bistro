(* This module is adapted from the format library [1] which is
 * distributed under GPL.
 *
 * [1] https://forge.ocamlcore.org/projects/format/
 *)

(* Copyright 2009 Tiphaine Turpin

   This file is part of Format.

   Format is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Format is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Format.  If not, see <http://www.gnu.org/licenses/>. *)

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

  let patt lexbuf offset text =
    Syntax.Gram.parse Syntax.patt_eoi (location lexbuf offset) (Stream.of_string text)

}

let no_get = ([^ '<' '#'] | '<'+ [^ '<' '-' '#'])* '<'*

let string_sequence = ([^ '#' '[' ']' '\\'] | '\\'+ [^ '\\' '!' '#' '[' ']'])+

rule token = parse

(* for *)
| "#!" (no_get as p) "<-" ([^ '#']* as e) '#'
    { FOR (patt lexbuf 2 p,
	   expr lexbuf (4 + String.length p) e) }

(* option *)
| "#?" (no_get as p) "<-" ([^ '#']* as e) '#'
    { OPT (patt lexbuf 2 p,
	   expr lexbuf (4 + String.length p) e) }

(* conditional *)
| "#?" ([^'#']* as e) '#'
    { IF (expr lexbuf 2 e) }

(* other data *)
| '#' (([^ '?' '[' '#' ':' ' ']* as typ) ':' ([^'#']* as e)) '#'
    { EXPR (typ, expr lexbuf (2 + String.length typ) e) }

(* "[" and "]" keywords *)
| "[" { LBR }
| "]" { RBR }

(* ordinary characters *)
| string_sequence as s { STRING (Camlp4.Struct.Token.Eval.string s) }
| "\\#" { STRING "#" }
| "\\[" { STRING "[" }
| "\\]" { STRING "]" }

| eof { EOF}

| "#DEST" { DEST }

| "#TMP" { TMP }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }










