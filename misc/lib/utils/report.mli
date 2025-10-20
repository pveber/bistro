
open Bistro

(** Markdown report *)
module Md : sig
  val svg : svg file -> Template_dsl.template
  val png : png file -> Template_dsl.template

  val to_html :
    Template_dsl.template ->
    html file
end
