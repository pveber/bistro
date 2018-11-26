(** Helper functions to represent paths as string lists. For absolute
    paths, the first element of the list is ["/"]. *)

type t = string list
[@@deriving sexp]

val compare : t -> t -> int
val of_string : string -> t
val to_string : t -> string

(** [make_relative ~from:dirA dirB] constructs a relative path that
    can be used to go from [dirA] to [dirB]. @raise
    [Invalid_argument] if [dirA] is relative. *)
val make_relative : ?from:string -> string -> t

