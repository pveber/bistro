open Bistro.Std
open Types

val tophat1 :
  ?num_threads:int ->
  ?color:bool ->
  Bowtie.index workflow ->
  [ `single_end of 'a fastq workflow list
  | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
  [`tophat_output] directory workflow

val tophat2 :
  ?num_threads:int ->
  Bowtie2.index workflow ->
  [ `single_end of 'a fastq workflow list
  | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
  [`tophat_output] directory workflow

val accepted_hits : ([`tophat_output] directory, bam) selector
