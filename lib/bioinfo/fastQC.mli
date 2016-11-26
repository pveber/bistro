open Bistro.Std
open Defs

type report = [`fastQC_report] directory

val run : 'a fastq workflow -> report workflow
val html_report : ([`fastQC_report], html) selector
val per_base_quality : ([`fastQC_report], png) selector
val per_base_sequence_content : ([`fastQC_report], png) selector
