open Bistro.Std
open Types

val sort :
  ?on:[`name | `position] ->
  bam workflow -> bam workflow
val indexed_bam_of_sam : sam workflow -> [ `indexed_bam ] directory workflow
val indexed_bam_of_bam : bam workflow -> [ `indexed_bam ] directory workflow
val indexed_bam_to_bam : ([ `indexed_bam ] directory, bam) selector
(* val bam_of_sam : sam workflow -> bam workflow *)
val sam_of_bam : bam workflow -> sam workflow

(* val rmdup : ?single_end_mode:bool -> bam workflow -> bam workflow *)
