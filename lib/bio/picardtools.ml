open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"picard-tools" ~tag:"2.8.1" () ]

let arg k v =
  seq ~sep:"" [ string k ; string "=" ; v ]

let markduplicates ?remove_duplicates indexed_bam =
  Workflow.shell ~descr:"picard.markduplicates" ~img ~mem:(Workflow.int (3 * 1024)) [
    mkdir_p dest ;
    cmd "PicardCommandLine" [
      string "MarkDuplicates" ;
      arg "INPUT" (dep @@ Samtools.indexed_bam_to_bam indexed_bam) ;
      arg "OUTPUT" (dest // "reads.bam") ;
      arg "METRICS_FILE" (dest // "dup_qc") ;
      string "VALIDATION_STRINGENCY=LENIENT" ;
      string "ASSUME_SORT_ORDER=coordinate" ;
      option (Printf.sprintf "REMOVE_DUPLICATES=%b" % string) remove_duplicates ;
    ]
  ]

let reads x = Workflow.select x ["reads.bam"]

let sort_bam_by_name bam =
  Workflow.shell ~descr:"picard.sort_bam_by_name" ~img ~mem:(Workflow.int (1 * 1024)) [
    cmd "PicardCommandLine" [
      string "SortSam" ;
      arg "INPUT" (dep bam) ;
      arg "OUTPUT" dest ;
      arg "SORT_ORDER" (string "queryname") ;
    ]
  ]
