open Bistro.Std
open Defs

val spades :
  ?single_cell:bool ->
  ?iontorrent:bool ->
  ?pe:[`sanger] fastq workflow list * [`sanger] fastq workflow list ->
  ?threads:int ->
  ?memory:int ->
  unit ->
  [`spades] directory workflow

val contigs : ([`spades], fasta) selector
val scaffolds : ([`spades], fasta) selector
