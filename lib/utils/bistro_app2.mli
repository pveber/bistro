open Core_kernel.Std
open Bistro.Std

type 'a t

val pure : 'a -> 'a t

val pureW : _ workflow -> (string -> 'a) -> 'a t

val app : ('a -> 'b) t -> 'a t -> 'b t

val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

val list : 'a t list -> 'a list t

val run :
  ?use_docker:bool ->
  ?np:int ->
  ?mem:int ->
  ?tmpdir:string ->
  'a t -> 'a
