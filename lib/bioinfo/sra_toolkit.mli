open Bistro.Std
open Defs

val env : docker_image

val fastq_dump : sra workflow -> [`sanger] fastq workflow

val fastq_dump_gz :
  [`id of string | `file of sra workflow] ->
  [`sanger] fastq gz workflow

val fastq_dump_pe : sra workflow -> [`sanger] fastq workflow * [`sanger] fastq workflow

val fastq_dump_pe_gz :
  [`id of string | `file of sra workflow] ->
  [`sanger] fastq gz workflow * [`sanger] fastq gz workflow

val fastq_dump_to_fasta : sra workflow -> fasta workflow
