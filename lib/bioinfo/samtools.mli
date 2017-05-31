open Bistro.Std
open Defs


type 'a input

val bam : bam input
val sam : sam input

val sort :
  ?on:[`name | `position] ->
  bam workflow -> bam workflow
val indexed_bam_of_sam : sam workflow -> indexed_bam workflow
val indexed_bam_of_bam : bam workflow -> indexed_bam workflow
val indexed_bam_to_bam : ([ `indexed_bam ], bam) selector
(* val bam_of_sam : sam workflow -> bam workflow *)
val sam_of_bam : bam workflow -> sam workflow

(* val rmdup : ?single_end_mode:bool -> bam workflow -> bam workflow *)

val view :
  ?b:bool ->
  ?_1:bool ->
  ?u:bool ->
  ?h:bool ->
  ?_H:bool ->
  ?c:bool ->
  ?_L: #bed3 workflow ->
  ?q:int ->
  ?m:int ->
  ?f:int ->
  ?_F:int ->
  ?_B:bool ->
  ?s:float ->
  ?_S:bool ->
  'a workflow ->
  sam workflow
