open Types

val package : package workflow

type report = [`fastQC_report] directory

val run : 'a fastq workflow -> report workflow
val html_report : report workflow -> html workflow
val per_base_quality : report workflow -> png workflow
val per_base_sequence_content : report workflow -> png workflow
