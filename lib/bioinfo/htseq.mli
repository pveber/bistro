open Bistro.Std
open Defs

class type count_tsv = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : int
end

val count :
  ?order:[`name | `position] ->
  ?mode:[`union | `intersection_strict | `intersection_nonempty] ->
  ?stranded:[` yes | `no | `reverse] ->
  ?feature_type:string ->
  ?minaqual:int ->
  ?idattribute:string ->
  [`sam of sam workflow | `bam of bam workflow] ->
  gff workflow ->
  count_tsv workflow
