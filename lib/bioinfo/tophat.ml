open Core_kernel.Std
open Types
open Bistro
open Bistro.EDSL_sh

let package = {
  pkg_name = "tophat" ;
  pkg_version = "2.1.1" ;
}

let tophat1 ?num_threads ?color index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow
    ?np:num_threads
    ~mem:(4 * 1024)
    ~pkgs:[Bowtie.package ; Samtools.package ; package] [
    cmd "tophat" [
      option (opt "--num-threads" int) num_threads ;
      option (flag string "--color") color ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let tophat2 ?num_threads index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow
    ?np:num_threads
    ~mem:(4 * 1024)
    ~pkgs:[Bowtie2.package ; Samtools.package ; package] [
    cmd "tophat2" [
      option (opt "--num-threads" int) num_threads ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let accepted_hits = selector ["accepted_hits.bam"]

let junctions = selector ["junctions.bed"]

