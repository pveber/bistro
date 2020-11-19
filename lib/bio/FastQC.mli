open Bistro

type report = [`fastQC] directory

val fastqc : #fastq file -> report
val fastqc_gz : #fastq gz file -> report
val html_report : report -> html file
val per_base_quality : report -> png file
val per_base_sequence_content : report -> png file
