(** Program representation at run time *)

type constant =
  | Constant_int of int

type expression = {
  hash : string option ;
  desc : exp_desc ;
}

and exp_desc =
  | Lconst of constant
  | Lvar of string
  | Lshell of expression Shell_ast.t

and t = (string * expression) list

module Exp : sig
  val int : int -> expression
  val shell : expression Shell_ast.t -> expression
end

val compile :
  Typedtree.structure ->
  t
