open Bistro.Std
open Bistro_engine

type 'a path = private Path of string

val ( / ) : 'a path -> ('a, 'b) Bistro.selector -> 'b path

type 'a t

val pure : 'a -> 'a t

val pureW : 'a workflow -> 'a path t

val app : ('a -> 'b) t -> 'a t -> 'b t

val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

val list : 'a t list -> 'a list t

val assoc : ('a * 'b t) list -> ('a * 'b) list t

val run :
  ?np:int ->
  ?mem:int ->
  ?logger:Scheduler.logger ->
  ?dag_dump:string ->
  ?keep_all:bool ->
  'a t -> 'a

type repo_item

val ( %> ) : string list -> _ workflow -> repo_item

val of_repo : outdir:string -> repo_item list -> unit t

module Syntax : sig
  module Let_syntax : sig
    type nonrec 'a t = 'a t
    val map  : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end
