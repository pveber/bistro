(** http://subread.sourceforge.net/ *)
open Bistro

class type count_table = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : string
  method f3 : int
  method f4 : int
  method f5 : [`Plus | `Minus]
  method f6 : int
  method f7 : int
end

val featureCounts :
  ?feature_type:string ->
  ?attribute_type:string ->
  ?strandness:[`Unstranded | `Stranded | `Reversely_stranded] ->
  ?chrAliases:csv file ->
  ?requireBothEndsMapped:bool ->
  ?countChimericFragments:bool ->
  ?minFragLength:int ->
  ?maxFragLength:int ->
  ?useMetaFeatures:bool ->
  ?allowMultiOverlap:bool ->
  ?fraction:float ->
  ?q:int ->
  ?nthreads:int ->
  gff file ->
  < format : [< `bam | `sam] ; .. > file -> (*FIXME: handle paired-hand, just add other file next to the other*)
  [`featureCounts] directory

val featureCounts_tsv : [`featureCounts] directory -> count_table file
val featureCounts_htseq_tsv : [`featureCounts] directory -> Htseq.count_tsv file
val featureCounts_summary : [`featureCounts] directory -> text file
