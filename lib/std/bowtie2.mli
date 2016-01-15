open Types

val package : package workflow

type index = [`bowtie2_index] directory

val bowtie2_build :
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
  fasta workflow ->
  index workflow

val bowtie2 :
  ?skip:int ->
  ?qupto:int ->
  ?trim5:int ->
  ?trim3:int ->
  ?preset:[`very_fast | `fast | `sensitive | `very_sensitive] ->
  ?_N:int ->
  ?_L:int ->
  ?ignore_quals:bool ->
  ?mode:[ `end_to_end | `local ] ->
  ?a:bool ->
  ?k:int ->
  ?_D:int ->
  ?_R:int ->
  ?minins:int ->
  ?maxins:int ->
  ?orientation:[`fr | `ff | `rf] ->
  ?no_mixed:bool ->
  ?no_discordant:bool ->
  ?dovetail:bool ->
  ?no_contain:bool ->
  ?no_overlap:bool ->
  ?threads:int ->
  ?seed:int ->
  ?fastq_format:'a Fastq.format ->
  index workflow ->
  [ `single_end of 'a fastq workflow list
  | `paired_end of 'a fastq workflow list * 'a fastq workflow list ] ->
  sam workflow
