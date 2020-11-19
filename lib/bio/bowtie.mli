open Bistro

val bowtie_build :
  ?packed:bool ->
  ?color:bool  ->
  fasta file -> [`bowtie_index] directory

val bowtie :
  ?l:int -> ?e:int -> ?m:int ->
  ?fastq_format:Fastq.format ->
  ?n:int -> ?v:int ->
  ?maxins:int ->
  ?additional_samples:Fastq_sample.t list ->
  [`bowtie_index] directory ->
  Fastq_sample.t ->
  sam file


val qual_option : Fastq.format -> string
val fastq_args :
  [`V1 | `V2] ->
  Fastq_sample.t list ->
  Shell_dsl.template
