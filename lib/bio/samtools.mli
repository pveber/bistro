open Bistro

type 'a format

val bam : bam format
val sam : sam format

val sort :
  ?on:[`name | `position] ->
  bam file -> bam file
val indexed_bam_of_sam : sam file -> [`indexed_bam] directory
val indexed_bam_of_bam : bam file -> [`indexed_bam] directory
val indexed_bam_to_bam : [`indexed_bam] directory -> bam file
val bam_of_sam : sam file -> bam file
val sam_of_bam : bam file -> sam file

(* val rmdup : ?single_end_mode:bool -> bam file -> bam file *)

val view :
  output:'o format ->
  (* ?_1:bool ->
   * ?u:bool -> *)
  ?h:bool ->
  ?_H:bool ->
  (* ?c:bool -> *)
  (* ?_L: #bed3 file -> *)
  ?q:int ->
  (* ?m:int ->
   * ?f:int ->
   * ?_F:int ->
   * ?_B:bool ->
   * ?s:float -> *)
  < file_kind : [`regular] ;
    format : [< `bam | `sam] ; .. > file ->
  'o file

val faidx :
  fasta file -> [`indexed_fasta] directory

val fasta_of_indexed_fasta :
  [`indexed_fasta] directory -> fasta file

val flagstat :
  < regular_file_t ; format : [< `bam | `sam] ; .. > file ->
  text file

val rmdup :
  ?single_end:bool ->
  [`indexed_bam] directory ->
  bam file
