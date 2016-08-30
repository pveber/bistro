open Bistro.Std
open Types

val meme_chip :
  ?meme_nmotifs:int ->
  ?meme_minw:int ->
  ?meme_maxw:int ->
  ?np:int ->
  fasta workflow ->
  [`meme_chip_output] directory workflow

val fimo :
  ?alpha: float ->
  ?bgfile: string ->
  ?max_stored_scores: int ->
  ?motif: string ->
  ?motif_pseudo: float ->
  ?qv_tresh:bool ->
  ?tresh: float ->
  [`meme_chip_output] directory workflow ->
  fasta workflow ->
  [`fimo_output] directory workflow
