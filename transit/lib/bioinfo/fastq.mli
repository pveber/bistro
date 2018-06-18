open Bistro.Std
open Defs

type _ format =
  | Sanger  : [`sanger] format
  | Solexa  : [`solexa] format
  | Phred64 : [`phred64] format

val to_sanger : 'a format -> 'a fastq workflow -> [`sanger] fastq workflow

val concat : 'a fastq workflow list -> 'a fastq workflow
val head : int -> 'a fastq workflow -> 'a fastq workflow
