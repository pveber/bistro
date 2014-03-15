%{

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


  open Bistro_script_ast
  open Printf

  let typ_of_string = function
    | "i" -> `int
    | "s" -> `string
    | "f" -> `float
    | "w" -> `workflow
    | x -> failwith (sprintf "Unknown conversion id %s" x)
%}

%token <string> STRING
%token <string * Camlp4.PreCast.Syntax.Ast.expr> EXPR
%token <Camlp4.PreCast.Ast.expr> IF
%token <Camlp4.PreCast.Ast.patt * Camlp4.PreCast.Ast.expr> OPT
%token <Camlp4.PreCast.Ast.patt * Camlp4.PreCast.Ast.expr> FOR
%token EOF LBR RBR
%token DEST TMP

%start script
%type <Bistro_script_ast.t> script

%%

script:
| items EOF { $1 }
;

items:
| list(item) { $1 }
;

item:
| STRING
    { String $1 }
| EXPR
    { let typ, expr = $1 in
      Expr (typ_of_string typ, expr) }
| DEST
    { Dest }
| TMP
    { Tmp }
| OPT LBR items RBR
    { let p, e = $1 in Opt (p, e, $3, []) }
| OPT LBR items RBR LBR items RBR
    { let p, e = $1 in Opt (p, e, $3, $6) }
| IF LBR items RBR
    { If ($1, $3, []) }
| IF LBR items RBR LBR items RBR
    { If ($1, $3, $6) }
| FOR LBR items RBR
    { let p, e = $1 in For (p, e, $3, []) }
| FOR LBR items RBR LBR items RBR
    { let p, e = $1 in For (p, e, $3, $6) }
;
