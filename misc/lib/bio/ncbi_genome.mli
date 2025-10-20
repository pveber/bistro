open Bistro
open Formats

val assembly_summary : tsv file

val fetch_assembly :
  genome_id:string ->
  assembly_id:string ->
  fasta gz file
