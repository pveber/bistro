open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"subread" ~tag:"1.6.3" () ]

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

let strandness_token = function
  | `Unstranded -> int 0
  | `Stranded -> int 1
  | `Reversely_stranded -> int 2

let featureCounts
    ?feature_type ?attribute_type ?strandness
    ?chrAliases ?requireBothEndsMapped ?countChimericFragments
    ?minFragLength ?maxFragLength ?useMetaFeatures
    ?allowMultiOverlap ?fraction
    ?q ?nthreads
    gff mapped_reads =
  Workflow.shell ~descr:"featureCounts" ~img ~np:(Option.value ~default:1 nthreads) [
    mkdir_p dest ;
    cmd "featureCounts" [
      option (opt "-t" string) feature_type ;
      option (opt "-g" string) attribute_type ;
      option (opt "-s" strandness_token) strandness ;
      option (opt "-A" dep) chrAliases ;
      option (flag string "-B") requireBothEndsMapped ;
      option (flag string "-C") countChimericFragments ;
      option (opt "-d" int) minFragLength ;
      option (opt "-D" int) maxFragLength ;
      option (flag string "-f") useMetaFeatures ;
      option (flag string "-O") allowMultiOverlap ;
      option (opt "-f" float) fraction ;
      option (opt "-Q" int) q ;
      option (opt "-T" (fun _ -> np)) nthreads ;
      opt "-a" dep gff ;
      opt "-o" Fn.id (dest // "counts.tsv") ;
      dep mapped_reads ;
    ]
  ]

let featureCounts_tsv o = Workflow.select o ["counts.tsv"]
let featureCounts_htseq_tsv o =
  Workflow.shell ~descr:"featureCounts_htseq_tsv" [
    pipe [
      cmd "sed" [ quote ~using:'\'' (string "1,2d") ; dep (featureCounts_tsv o) ] ;
      cmd "awk" ~stdout:dest [
        quote ~using:'\'' (string "{print $1,$7}") ;
        string "OFS='\t'" ;
      ]
    ]
  ]
let featureCounts_summary o = Workflow.select o ["counts.tsv.summary"]
