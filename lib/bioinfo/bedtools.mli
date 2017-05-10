open Bistro.Std
open Defs

val env : Bistro.docker_image

type 'a input

val bed : #bed3 input
val gff : gff input


module Cmd : sig
  val slop :
    ?strand:bool ->
    ?header:bool ->
    mode:[
      | `both of int
      | `left of int
      | `right of int
      | `both_pct of float
      | `left_pct of float
      | `right_pct of float
    ] ->
    'a input ->
    'a workflow ->
    Ucsc_gb.chrom_sizes workflow ->
    Bistro.command
end


val slop :
  ?strand:bool ->
  ?header:bool ->
  mode:[
    | `both of int
    | `left of int
    | `right of int
    | `both_pct of float
    | `left_pct of float
    | `right_pct of float
  ] ->
  'a input ->
  'a workflow ->
  Ucsc_gb.chrom_sizes workflow ->
  'a workflow


val intersect :
  ?ubam:bool ->
  ?wa:bool ->
  ?wb:bool ->
  ?loj:bool ->
  ?wo:bool ->
  ?wao:bool ->
  ?u:bool ->
  ?c:bool ->
  ?v:bool ->
  ?f:float ->
  ?_F:float ->
  ?r:bool ->
  ?e:bool ->
  ?s:bool ->
  ?_S:bool ->
  ?split:bool ->
  ?sorted:bool ->
  ?g:Ucsc_gb.chrom_sizes workflow ->
  ?header:bool ->
  ?filenames:bool ->
  ?sortout:bool ->
  'a workflow ->
  'a workflow list ->
  'a workflow
