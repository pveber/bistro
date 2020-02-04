open Bistro

type t
type cell

val section : string -> cell
val subsection : string -> cell
val text : string -> cell
val pdf : pdf file -> cell
val svg : svg file -> cell
val png : png file -> cell

val make :
  title:string ->
  cell list ->
  t

val render : t -> html file
