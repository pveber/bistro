(** Markdown report *)

open Bistro
open Bistro_engine

type t

val make :
  title:string ->
  Template_dsl.template ->
  t

val svg : svg file -> Template_dsl.template
val png : png file -> Template_dsl.template

val to_html : t -> html file

val build :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  output:string -> t -> unit Lwt.t

val build_main  :
  ?np:int ->
  ?mem:[`GB of int] ->
  ?loggers:Logger.t list ->
  ?allowed_containers:[`Docker | `Singularity] list ->
  ?bistro_dir:string ->
  ?collect:bool ->
  output:string -> t -> unit
