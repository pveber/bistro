open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"tophat" ~tag:"2.1.1" () ]

let tophat1 ?color index fqs =
  let args = match fqs with
    | SE_or_PE.Single_end fqs -> list dep ~sep:"," fqs
    | Paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  Workflow.shell ~img ~np:8 ~mem:(Workflow.int (4 * 1024)) ~descr:"tophat" [
    cmd "tophat" [
      string "--bowtie1" ;
      opt "--num-threads" Fn.id np ;
      option (flag string "--color") color ;
      opt "--output-dir" Fn.id dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let tophat2 index fqs =
  let args = match fqs with
    | SE_or_PE.Single_end fqs -> list dep ~sep:"," fqs
    | Paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  Workflow.shell ~img ~np:8 ~mem:(Workflow.int (4 * 1024)) ~descr:"tophat2" [
    cmd "tophat2" [
      opt "--num-threads" Fn.id np ;
      opt "--output-dir" Fn.id dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let accepted_hits x = Workflow.select x ["accepted_hits.bam"]

let junctions x = Workflow.select x ["junctions.bed"]
