open Core_kernel
open Bistro.Std
open Bistro.EDSL
open Defs

type 'a sample = {
  id : string ;
  tissue : string ;
  factor : string ;
  replicate : string ;
  bam : bam workflow ;
  peaks : (#bed3 as 'a) workflow ;
}

type output = [`ChIPQC] directory

let env = docker_image ~account:"pveber" ~name:"bioconductor" ~tag:"3.3" ()

let sample_sheet samples =
  let header = string "SampleID,Tissue,Factor,Replicate,bamReads,Peaks" in
  let line { id ; tissue ; factor ; replicate ; bam ; peaks } =
    seq ~sep:"," [ string id ; string tissue ; string factor ; string replicate ; dep bam ; dep peaks ]
  in
  seq ~sep:"\n" (header :: List.map samples ~f:line)

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
      string {|ChIPQCreport(experiment,reportFolder="|} ;
      dest ;
      string {|")|} ;
    ]
  ]

let run samples =
  workflow ~descr:"ChIPQC" [
    cmd "Rscript" ~env [ file_dump (rscript (sample_sheet samples)) ] ;
  ]
