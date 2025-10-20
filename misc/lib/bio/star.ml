open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"flemoine" ~name:"star" () ]

let mem_in_bytes = seq ~sep:" " [string "$((" ; mem ; string " * 1024 * 1024))$"]

let genomeGenerate fa =
  Workflow.shell ~img ~descr:"star.index" ~np:8 ~mem:(Workflow.int (30 * 1024)) [
    mkdir_p dest ;
    cmd "STAR" [
      opt "--runThreadN" Fn.id np ;
      opt "--runMode" string "genomeGenerate" ;
      opt "--genomeDir" Fn.id dest ;
      opt "--genomeFastaFiles" dep fa ;
      opt "--limitGenomeGenerateRAM" Fn.id mem_in_bytes ;
    ]
  ]

let fq_args = function
  | SE_or_PE.Single_end fq -> [ dep fq ]
  | Paired_end (fq1, fq2) ->
    [dep fq1 ; dep fq2]

let samStrandField = function
  | `None -> string "None"
  | `intronMotif -> string "intronMotif"


let sorted_mapped_reads x = Workflow.select x ["sorted.bam"]

let alignReads ?(max_mem = `GB 8)
    ?outFilterMismatchNmax
    ?outFilterMultimapNmax
    ?outSAMstrandField
    ?alignIntronMax
    idx fqs =
  let `GB max_mem = max_mem in
  Workflow.shell ~descr:"star.map" ~img ~np:8 ~mem:(Workflow.int (max_mem * 1024)) [
    mkdir_p dest ;
    cmd "STAR" ~stdout:(dest // "sorted.bam") [
      opt "--outFileNamePrefix" Fn.id (dest // "star") ;
      opt "--runThreadN" Fn.id np ;
      option (opt "--outSAMstrandField" samStrandField) outSAMstrandField ;
      option (opt "--outFilterMismatchNmax" int) outFilterMismatchNmax ;
      option (opt "--outFilterMultimapNmax" int) outFilterMultimapNmax ;
      opt "--genomeDir" dep idx ;
      opt "--readFilesIn" Fn.id (seq ~sep:" " (fq_args fqs)) ;
      opt "--outSAMunmapped" string "None" ;
      opt "--outStd" string "SAM" ;
      opt "--genomeLoad" string "NoSharedMemory" ;
      option (opt "--alignIntronMax" int) alignIntronMax ;
      (* opt "--limitBAMsortRAM" Fn.id mem_in_bytes ; *)
    ]
  ]
  |> sorted_mapped_reads
