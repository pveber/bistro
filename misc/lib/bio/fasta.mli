open Bistro
open Formats

type t = fasta file

val concat :
  [`plain of fasta file | `gziped of fasta gz file] list ->
  fasta file

val concat_gz :
  [`plain of fasta file | `gziped of fasta gz file] list ->
  fasta gz file
