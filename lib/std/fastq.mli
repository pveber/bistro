open Types

type _ format =
  | Sanger  : [`sanger] format
  | Solexa  : [`solexa] format
  | Phred64 : [`phred64] format

val sanger_of_solexa : [`solexa] fastq workflow -> [`sanger] fastq workflow
val sanger_of_solexa : [`phred64] workflow -> [`sanger] fastq workflow
val to_sanger : 'a format -> 'a fastq workflow -> [`sanger] fastq workflow

val concat : 'a fastq workflow list -> 'a fastq workflow
