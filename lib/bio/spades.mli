(** {{http://cab.spbu.ru/files/release3.14.0/manual.html}SPADES assembler} *)

open Bistro

val spades :
  ?single_cell:bool ->
  ?iontorrent:bool ->
  ?threads:int ->
  ?memory:int ->
  Fastq_sample.t list ->
  [`spades] directory

val contigs : [`spades] directory -> fasta file
val scaffolds : [`spades] directory -> fasta file

val rnaspades :
  ?threads:int ->
  ?memory:int ->
  ?ss:[`fr | `rf] ->
  Fastq_sample.t list ->
  [`rnaspades] directory

val transcripts : [`rnaspades] directory -> fasta file
val hard_filtered_transcripts : [`rnaspades] directory -> fasta file
val soft_filtered_transcripts : [`rnaspades] directory -> fasta file
