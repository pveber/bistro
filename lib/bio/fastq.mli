open Bistro
open Formats

type format =
  | Sanger
  | Solexa
  | Phred64

val concat : (#fastq as 'a) file list -> 'a file
val head : int -> (#fastq as 'a) file -> 'a file
val zhead : int -> (#fastq as 'a) gz file -> 'a file
