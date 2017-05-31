open Bistro.Std
open Defs


type 'a format

val bam : bam format
val sam : sam format

val sort :
  ?on:[`name | `position] ->
  bam workflow -> bam workflow
val indexed_bam_of_sam : sam workflow -> indexed_bam workflow
val indexed_bam_of_bam : bam workflow -> indexed_bam workflow
val indexed_bam_to_bam : ([ `indexed_bam ], bam) selector
val bam_of_sam : sam workflow -> bam workflow
val sam_of_bam : bam workflow -> sam workflow

(* val rmdup : ?single_end_mode:bool -> bam workflow -> bam workflow *)
(*
val view :
  input_format:'i format ->
  output_format:'o format ->
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
  'i workflow ->
  'o workflow
*)
