open Bistro

val merge :
  ?min_length:int ->
  (string * fasta file) list -> fasta file

val cisa :
  genome_size:int ->
  fasta file ->
  fasta file
