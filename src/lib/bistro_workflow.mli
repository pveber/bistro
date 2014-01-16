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
and cmd =
| S : string -> cmd
| I : int -> cmd
| F : float -> cmd
| W : 'a t -> cmd (* workflow *)
| L : cmd list -> cmd
| Q : cmd * char -> cmd (* inside a quotation, quotes are not allowed *)
| D : cmd (* destination *)
| TMP : cmd
| E : cmd (* empty word *)
and 'a file = [`file of 'a] t
and 'a directory = [`directory of 'a] t
and package = [`package] directory

  (**
     Examples of command:
     S[A"touch" ; D]
     S[A"gunzip" ; A"-c" ; W archive ; A">" ; D]
  *)

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
