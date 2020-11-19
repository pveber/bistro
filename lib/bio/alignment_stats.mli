open Bistro

val bamstats : bam file -> text file
val fragment_length_stats : bam file -> text file
val chrstats : bam file -> text file
val summary :
  sample_name:('a -> string) ->
  mapped_reads:('a -> bam file) ->
  'a list ->
  html file
