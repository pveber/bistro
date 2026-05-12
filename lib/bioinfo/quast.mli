open Bistro.Std
open Bistro_bioinfo.Std

type quast_output = [`quast_output] directory

val quast :
  ?reference:fasta workflow ->
  ?labels:string list ->
  fasta workflow list ->
  quast_output workflow
