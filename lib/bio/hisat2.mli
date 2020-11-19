open Bistro

val img : container_image list

val hisat2_build :
  ?large_index:bool ->
  ?noauto:bool ->
  ?packed:bool ->
  ?bmax:int ->
  ?bmaxdivn:int ->
  ?dcv:int ->
  ?nodc:bool ->
  ?noref:bool ->
  ?justref:bool ->
  ?offrate:int ->
  ?ftabchars:int ->
  ?seed:int ->
  ?cutoff:int ->
  fasta file ->
  [`hisat2_index] directory


val hisat2 :
  ?skip:int ->
  ?qupto:int ->
  ?trim5:int ->
  ?trim3:int ->
  ?fastq_format:Fastq.format ->
  ?k:int ->
  ?minins:int ->
  ?maxins:int ->
  ?orientation:[`fr | `ff | `rf] ->
  ?no_mixed:bool ->
  ?no_discordant:bool ->
  ?seed:int ->
  ?additional_samples:Fastq_sample.t list ->
  [`hisat2_index] directory ->
  Fastq_sample.t ->
  sam file
