open Bistro.Std
open Defs

val fastq_dump : sra workflow -> [`sanger] fastq workflow

val fastq_dump_pe : sra workflow -> [`sanger] fastq workflow * [`sanger] fastq workflow

val fastq_dump_to_fasta : sra workflow -> fasta workflow
