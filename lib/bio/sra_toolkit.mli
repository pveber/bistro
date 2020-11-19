open Bistro

val img : container_image list

type 'a output

val fastq : fastq output
val fastq_gz : fastq gz output
val fasta : fasta output

val fastq_dump :
  ?minReadLen:int ->
  ?_N_:int ->
  ?_X_:int ->
  ?defline_qual:string ->
  ?defline_seq:string ->
  'a output ->
  [`id of string | `idw of string workflow | `file of sra file] ->
  'a file

val fastq_dump_pe :
  ?minReadLen:int ->
  ?_N_:int ->
  ?_X_:int ->
  ?defline_qual:string ->
  ?defline_seq:string ->
  'a output ->
  [`id of string | `idw of string workflow | `file of sra file] ->
  'a file * 'a file
