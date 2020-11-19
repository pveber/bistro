open Bistro
open Formats

val quast :
  ?reference:fasta file ->
  ?labels:string list ->
  fasta file list ->
  [`quast] directory
