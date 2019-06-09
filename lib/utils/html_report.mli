open Bistro

type t
type cell

val section : string -> cell
val subsection : string -> cell
val text : string -> cell
val pdf : pdf pworkflow -> cell
val svg : svg pworkflow -> cell
val png : png pworkflow -> cell

val make :
  title:string ->
  cell list ->
  t

val render : t -> html pworkflow
