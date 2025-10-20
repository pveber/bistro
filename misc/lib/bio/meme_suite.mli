open Bistro
open Formats

val meme :
  ?nmotifs:int ->
  ?minw:int ->
  ?maxw:int ->
  ?revcomp:bool ->
  ?maxsize:int ->
  ?alphabet:[`dna | `rna | `protein] ->
  (* ?threads:int -> *)
  fasta file ->
  [`meme] directory

val meme_logo :
  [`meme] directory ->
  ?rc:bool ->
  int ->
  png file

val meme_chip :
  ?meme_nmotifs:int ->
  ?meme_minw:int ->
  ?meme_maxw:int ->
  (* ?np:int -> *)
  fasta file ->
  [`meme_chip] directory

(** http://meme-suite.org/doc/fimo.html?man_type=web *)
val fimo :
  ?alpha: float ->
  ?bgfile:text file ->
  ?max_stored_scores: int ->
  ?max_strand:bool ->
  ?motif:string ->
  ?motif_pseudo:float ->
  ?no_qvalue:bool ->
  ?norc:bool ->
  ?parse_genomic_coord:bool ->
  ?prior_dist:text file ->
  ?psp:text file ->
  ?qv_thresh:bool ->
  ?thresh: float ->
  [`meme] directory ->
  fasta file ->
  [`fimo] directory
