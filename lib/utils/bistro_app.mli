open Bistro.Std
open Bistro_engine

type 'a path = private Path of string

val ( / ) : 'a path -> ('a, 'b) Bistro.selector -> 'b path

type 'a t

val pure : 'a -> 'a t

val pureW : 'a workflow -> ('a path -> 'b) -> 'b t

val app : ('a -> 'b) t -> 'a t -> 'b t

val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t

val list : 'a t list -> 'a list t

val run :
  ?config:Task.config ->
  ?np:int ->
  ?mem:int ->
  ?logger:Scheduler.logger ->
  'a t -> 'a

type repo_item

val ( %> ) : string list -> _ workflow -> repo_item

val of_repo : outdir:string -> repo_item list -> unit t
