open Bistro
open Bistro_engine

type item

type t = item list

val ( %> ) : string list -> 'a path workflow -> item

val item : string list -> 'a path workflow -> item

val precious_item : _ path workflow -> item

val singleton : string -> 'a path workflow -> t

val add_prefix : string list -> t -> t

val shift : string -> t -> t

val build_main  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  outdir:string -> t -> unit

val build :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  outdir:string -> t -> unit Lwt.t

val to_workflow :
  outdir:string ->
  t ->
  unit workflow

val cache_clip_dry_run :
  bistro_dir:string ->
  t ->
  int * int * int * int

val cache_clip :
  bistro_dir:string ->
  t ->
  unit
