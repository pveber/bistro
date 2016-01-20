open Bistro.Std
open Types

val meme_chip :
  ?meme_nmotifs:int ->
  ?meme_minw:int ->
  ?meme_maxw:int ->
  ?meme_p:int ->
  fasta workflow ->
  [`meme_chip_output] directory workflow
