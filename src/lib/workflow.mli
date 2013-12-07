type path = string

type 'a t

val digest : 'a t -> string


type cmd =
| A : string -> cmd (* atom = quoted string *)
| W : 'a t -> cmd (* workflow *)
| S : cmd list -> cmd
| Q : cmd list -> cmd (* inside a quotation, nothing is quoted *)
| D : cmd (* destination *)
| N : cmd (* nothing *)
  (**
     Examples:
     S[A"touch" ; D]
     S[A"gunzip" ; A"-d" ; W archive]
  *)
(* val string_of_command : ('a t -> string) -> cmd -> string *)

(** {5 Constructors} *)
val input : path -> 'a t
val make : cmd list -> 'a t
val select : [`dir of _] t -> path -> _ t


val depends_on : 'a t -> 'b t -> 'b t
