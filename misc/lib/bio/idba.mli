open Bistro
open Formats

val fq2fa :
  ?filter:bool ->
  [ `Se of fastq file
  | `Pe_merge of fastq file * fastq file
  | `Pe_paired of fastq file ] ->
  fasta file

val idba_ud : ?mem_spec:int -> fasta file -> [`idba] directory

val idba_ud_contigs : [`idba] directory -> fasta file
val idba_ud_scaffolds : [`idba] directory -> fasta file
