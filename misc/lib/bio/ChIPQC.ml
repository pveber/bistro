open Core
open Bistro
open Bistro.Shell_dsl
open Formats

type 'a sample = {
  id : string ;
  tissue : string ;
  factor : string ;
  replicate : string ;
  bam : [`indexed_bam] directory ;
  peaks : (#bed3 as 'a) file ;
}

let img = [ docker_image ~account:"pveber" ~name:"bioconductor" ~tag:"3.8" () ]

let sample_sheet samples =
  let header = string "SampleID,Tissue,Factor,Replicate,bamReads,Peaks" in
  let line { id ; tissue ; factor ; replicate ; bam ; peaks } =
    seq ~sep:"," [ string id ; string tissue ; string factor ; string replicate ; dep (Samtools.indexed_bam_to_bam bam) ; dep peaks ]
  in
  seq ~sep:"\n" (header :: List.map ~f:line samples)

let rscript sample_sheet =
  seq ~sep:"\n" [
    string "library(ChIPQC)" ;
    seq ~sep:"" [
      string {|samples = read.csv("|} ;
      file_dump sample_sheet ;
      string {|")|} ;
    ] ;
    string "experiment = ChIPQC(samples)" ;
    seq ~sep:"" [
      string {|ChIPQCreport(experiment,facet=F,reportFolder="|} ;
      dest ;
      string {|")|} ;
    ]
  ]

let run samples =
  Workflow.shell ~descr:"ChIPQC" ~img [
    cmd "Rscript" [ file_dump (rscript (sample_sheet samples)) ] ;
  ]
