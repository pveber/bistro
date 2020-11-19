open Bistro

val genomeGenerate : fasta file -> [`star_index] directory

val alignReads :
  ?max_mem:[`GB of int] ->
  ?outFilterMismatchNmax:int ->
  ?outFilterMultimapNmax:int ->
  ?outSAMstrandField:[`None | `intronMotif] ->
  ?alignIntronMax:int ->
  [`star_index] directory ->
  fastq file SE_or_PE.t ->
  bam file
