(** Markdown report *)

open Bistro

type t

val make :
  title:string ->
  Template_dsl.template ->
  t

val svg : svg file -> Template_dsl.template
val png : png file -> Template_dsl.template

val to_html : t -> html file
