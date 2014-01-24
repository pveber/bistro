type path = string

type 'a t = private u
and u =
| Input of path (* input file *)
| Rule of rule (* commands to build a target *)
| Select of u * path (* access to a file in a directory *)
and rule = {
  cmds : cmd list ;
  deps : u list ;
  np : int ; (* required number of processors *)
  mem : int ; (* required memory *)
  timeout : duration ; (* maximum allowed running time *)
}
and duration = [`minute | `hour | `day | `week | `month]
and cmd = Cmd of token list
and token =
| S : string -> token
| I : int -> token
| F : float -> token
| W : 'a t -> token
| L : token list -> token
| D : token
| TMP : token
| Q : token * char -> token

  (**
     Examples of command:
     prog "htseq"
       @@ opt (arg string ~o:"-m") mode
       @@ flag "-m"
       @@ arg string
  *)

module Cmd : sig
  type 'a t = (cmd -> 'a) -> 'a
  val cmd : string -> 'a t
  val string : cmd -> string -> 'a t
  val int : cmd -> int -> 'a t
  val dest : cmd -> 'a t
  val opt : cmd -> (cmd -> 'a -> 'b t) -> 'a option -> 'b t
  val opt2 : cmd -> (cmd -> 'a -> 'b -> 'c t) -> 'a -> 'b option -> 'c t
  val arg : cmd -> (cmd -> 'a -> 'b -> 'c) -> 'a -> 'b -> 'c
  val argp : cmd -> (cmd -> 'a -> 'b -> 'c) -> string -> 'a -> 'b -> 'c
  val stdout_to : cmd -> 'a t

  val make : ((cmd -> cmd) -> 'a) -> 'a
  val script : ((cmd -> cmd) -> 'a) list -> 'a list
end

val export_PATH_cmd : [`directory of [`package]] t list -> cmd

(** {5 Observers}*)
val digest : u -> string
val exec_cmd : dest:string -> tmp:string -> (u -> path) -> cmd -> string
val deps : u -> u list

val depth_first_traversal : _ t -> init:'a -> f:(u -> 'a -> 'a) -> 'a

(** {5 Constructors} *)
val input : path -> 'a t
val make :
  ?np:int ->
  ?mem:int ->
  ?timeout:duration ->
  cmd list -> 'a t
val select : [`dir of _] t -> path -> _ t


val depends : 'a t -> on:_ t -> 'a t
