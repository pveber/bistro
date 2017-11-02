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

val create :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?logger:Scheduler.logger ->
  ?keep_all:bool ->
  ?bistro_dir:string ->
  'a t -> ('a, string) result Lwt.t

val run :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?logger:Scheduler.logger ->
  ?keep_all:bool ->
  ?bistro_dir:string ->
  'a t -> 'a

module Syntax : sig
  module Let_syntax : sig
    type nonrec 'a t = 'a t
    val map  : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end
