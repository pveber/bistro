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

val build_main  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_environments:[`Docker | `Singularity | `Guix] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  outdir:string -> t -> unit

val build :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_environments:[`Docker | `Singularity | `Guix] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  outdir:string -> t -> unit Lwt.t

val to_workflow :
  outdir:string ->
  t ->
  unit workflow
