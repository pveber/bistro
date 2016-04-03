open Bistro.Std
open Types

type index = [`bowtie_index] directory

val package : package

val bowtie_build :
  ?packed:bool ->
  ?color:bool  ->
  fasta workflow -> index workflow

val bowtie :
  ?l:int -> ?e:int -> ?m:int ->
  ?fastq_format:'a Fastq.format ->
  ?n:int -> ?v:int ->
  ?maxins:int ->
  index workflow ->
  [ `single_end of 'a fastq workflow list
  | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
  sam workflow
