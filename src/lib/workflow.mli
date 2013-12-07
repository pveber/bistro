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
| A : string -> cmd (* atom = string *)
| W : 'a t -> cmd (* workflow *)
| S : cmd list -> cmd
| Q : cmd -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| N : cmd (* nothing *)

  (**
     Examples of command:
     S[A"touch" ; D]
     S[A"gunzip" ; A"-c" ; W archive ; A">" ; D]
  *)

(** {5 Observers}*)
val digest : u -> string
val path :
  cache_dir:string ->
  u -> path
val exec_cmd : string -> (u -> path) -> cmd -> string list


(** {5 Constructors} *)
val input : path -> 'a t
val make : cmd list -> 'a t
val select : [`dir of _] t -> path -> _ t


val depends : 'a t -> on:_ t -> 'a t
