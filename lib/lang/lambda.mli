(** Program representation at run time *)

type path =
  | FS of string
  | Cache of string

type constant =
  | Constant_int of int
  | Constant_path of path

type expression = {
  hash : string option ;
  desc : exp_desc ;
}

and exp_desc =
  | Lconst of constant
  | Lvar of string
  | Lshell of expression Shell_ast.t
  | Llam of string * expression

and t = (string * expression) list

module Exp : sig
  val int : int -> expression
  val path : path -> expression
  val shell : expression Shell_ast.t -> expression
  val lam : string -> expression -> expression
end

val compile :
  Typedtree.structure ->
  t
