open Core_kernel.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"tophat" ~tag:"2.1.1" ()

let tophat1 ?color index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow ~np:8 ~mem:(4 * 1024) ~descr:"tophat" [
    cmd ~env "tophat" [
      string "--bowtie1" ;
      opt "--num-threads" ident np ;
      option (flag string "--color") color ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let tophat2 index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow ~np:8 ~mem:(4 * 1024) ~descr:"tophat2" [
    cmd ~env "tophat2" [
      opt "--num-threads" ident np ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let accepted_hits = selector ["accepted_hits.bam"]

let junctions = selector ["junctions.bed"]

