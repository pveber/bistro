open Bistro
open Bistro_engine

type item

type t = item list

val ( %> ) : string list -> 'a path workflow -> item

val item : string list -> 'a path workflow -> item

val singleton : string -> 'a path workflow -> t

val items :
  string list ->
  prefix:string ->
  ?ext:string ->
  'a path list workflow ->
  item

val add_prefix : string list -> t -> t

val shift : string -> t -> t

val build  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?keep_all:bool ->
  ?use_docker:bool ->
  ?bistro_dir:string ->
  outdir:string -> t -> unit Lwt.t

val to_workflow :
  outdir:string ->
  t ->
  unit workflow
