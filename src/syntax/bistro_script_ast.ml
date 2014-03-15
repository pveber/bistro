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

open Camlp4.PreCast.Syntax.Ast

type t = item list
and item =
| String of string
| Expr of expr_ty * expr
| Dest
| Tmp
| If of expr * t * t
| Opt of patt * expr * t * t
| For of patt * expr * t * t

and expr_ty = [
| `int
| `string
| `float
| `workflow
]
