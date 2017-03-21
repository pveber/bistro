open Bistro.Std
open Bistro_bioinfo
open Defs

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
  'a fastq workflow ->
  (string * fasta workflow) list ->
  [ `fastq_screen ] directory workflow

val html_report : ([ `fastq_screen ], html) selector
