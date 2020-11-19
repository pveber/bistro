open Bistro

val fastq_screen :
  ?bowtie2_opts:string ->
  ?filter: [ `Not_map | `Uniquely | `Multi_maps | `Maps | `Not_map_or_Uniquely | `Not_map_or_Multi_maps | `Ignore ] list ->
  ?illumina:bool ->
  ?nohits:bool ->
  ?pass:int ->
  ?subset:int ->
  ?tag:bool ->
  ?threads:int ->
  ?top: [ `top1 of int | `top2 of int * int ] ->
  ?lightweight:bool ->
  Fastq_sample.t ->
  (string * fasta file) list ->
  [`fastq_screen] directory

val html_report : [`fastq_screen] directory -> html file
