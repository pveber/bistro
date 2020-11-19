open Bistro
open Formats

val img : container_image list

val markduplicates :
  ?remove_duplicates:bool ->
  [`indexed_bam] directory ->
  [`picard_markduplicates] directory

val reads :
  [`picard_markduplicates] directory ->
  bam file

val sort_bam_by_name :
  bam file ->
  bam file
