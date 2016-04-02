open Bistro.Std
open Types

val package : package

val fastq_dump : sra workflow -> [`sanger] fastq workflow

val fastq_dump_pe : sra workflow -> [`sanger] fastq workflow * [`sanger] fastq workflow
