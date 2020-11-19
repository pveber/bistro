open Bistro
open Formats

val tophat1 :
  ?color:bool ->
  [`bowtie_index] directory ->
  #fastq file list SE_or_PE.t ->
  [`tophat] directory

val tophat2 :
  [`bowtie2_index] directory ->
  #fastq file list SE_or_PE.t ->
  [`tophat] directory

val accepted_hits : [`tophat] directory -> bam file
val junctions : [`tophat] directory -> bed6 file
