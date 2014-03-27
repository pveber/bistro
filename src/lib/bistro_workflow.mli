type path = string

type 'a t = private u
and u =
| Input of path (* input file *)
| Rule of rule (* commands to build a target *)
| Select of u * path (* access to a file in a directory *)
and rule = {
  script : script ;
  deps : u list ;
  np : int ; (* required number of processors *)
  mem : int ; (* required memory *)
  timeout : duration ; (* maximum allowed running time *)
}
and duration = [`minute | `hour | `day | `week | `month]
and script = token list
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

module Script : sig
  type t = script
  type 'a cons = (t -> 'a) -> 'a

  val begin_ : 'a cons
  val cmd : t -> string -> 'a cons
  val string : t -> string -> 'a cons
  val int : t -> int -> 'a cons
  val dest : t -> 'a cons
  val opt : t -> (t -> 'a -> 'b cons) -> 'a option -> 'b cons
  val opt2 : t -> (t -> 'a -> 'b -> 'c cons) -> 'a -> 'b option -> 'c cons
  val arg : t -> (t -> 'a -> 'b -> 'c) -> 'a -> 'b -> 'c
  val argp : t -> (t -> 'a -> 'b -> 'c) -> string -> 'a -> 'b -> 'c
  val stdout_to : t -> 'a cons
  val end_ : t -> t

  val to_string : dest:string -> tmp:string -> (u -> path) -> t -> string
end

(* val export_PATH_cmd : [`directory of [`package]] t list -> cmd *)

(** {5 Observers}*)
val digest : u -> string
val deps : u -> u list

val depth_first_traversal : _ t -> init:'a -> f:(u -> 'a -> 'a) -> 'a

(** {5 Constructors} *)
val input : path -> 'a t
val make :
  ?np:int ->
  ?mem:int ->
  ?timeout:duration ->
  Script.t -> 'a t
val select : [`directory of _] t -> path -> _ t


val depends : 'a t -> on:_ t -> 'a t
