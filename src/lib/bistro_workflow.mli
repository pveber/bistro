type path = string

type 'a t = private u
and u =
| Input of path
| Rule of rule
| Select of u * path
and rule = {
  cmds : cmd list ;
  deps : u list ;
}
and cmd =
| S : string -> cmd
| I : int -> cmd
| F : float -> cmd
| W : 'a t -> cmd (* workflow *)
| L : cmd list -> cmd
| Q : cmd -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| E : cmd (* empty word *)

  (**
     Examples of command:
     S[A"touch" ; D]
     S[A"gunzip" ; A"-c" ; W archive ; A">" ; D]
  *)

(** {5 Observers}*)
val digest : u -> string
val exec_cmd : string -> (u -> path) -> cmd -> string list
val deps : u -> u list

val depth_first_traversal : _ t -> init:'a -> f:(u -> 'a -> 'a) -> 'a

(** {5 Constructors} *)
val input : path -> 'a t
val make : cmd list -> 'a t
val select : [`dir of _] t -> path -> _ t


val depends : 'a t -> on:_ t -> 'a t
