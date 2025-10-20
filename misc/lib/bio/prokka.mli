open Bistro
open Formats

val run :
  ?prefix:string ->
  ?addgenes:bool ->
  ?locustag:string ->
  ?increment:int ->
  ?gffver:string ->
  ?compliant:bool ->
  ?centre:string ->
  ?genus:string ->
  ?species:string ->
  ?strain:string ->
  ?plasmid:string ->
  ?kingdom:string ->
  ?gcode:int ->
  ?gram: [ `Plus | `Minus ] ->
  ?usegenus:bool ->
  ?proteins:string ->
  ?hmms:string ->
  ?metagenome:bool ->
  ?rawproduct:bool ->
  ?fast:bool ->
  ?threads:int ->
  ?mincontiglen:int ->
  ?evalue:float ->
  ?rfam:bool ->
  ?norrna:bool ->
  ?notrna:bool ->
  ?rnammer:bool ->
  fasta file ->
  [`prokka] directory
