open Bistro
open Bistro.Template_dsl

type t
type expr
type arg


val make : expr list -> template

val dest : expr
val tmp : expr
val source : string -> expr
val call : string -> arg list -> expr
val string : string -> expr
val int : int -> expr
val float : float -> expr
val dep : _ pworkflow -> expr
val ints : int list -> expr
val ints_dep : int list workflow -> expr
val floats : float list -> expr
val floats_dep : float list workflow -> expr
val strings : string list -> expr
val strings_dep : string list workflow -> expr
val deps : _ pworkflow list -> expr
val arg : ?l:string -> expr -> arg
val assign : string -> expr -> expr

val workflow :
  ?descr:string ->
  ?np:int ->
  ?mem:int workflow ->
  ?img:Shell_dsl.container_image list ->
  template ->
  'a pworkflow

val workflow' :
  ?descr:string ->
  ?np:int ->
  ?mem:int workflow ->
  ?img:Shell_dsl.container_image list ->
  expr list ->
  'a pworkflow
