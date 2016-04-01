open Core_kernel.Std
open Bistro
open Types
open Bistro.EDSL_sh

let package = {
  pkg_name = "samtools" ;
  pkg_version = "0.1.19" ;
}

let samtools subcmd args =
  cmd "samtools" (string subcmd :: args)

let sam_of_bam bam =
  workflow ~pkgs:[package] ~descr:"samtools.sam_of_bam" [
    samtools "view" [
      opt "-o" ident dest ;
      dep bam ;
    ]
  ]

let indexed_bam_of_sam sam =
  workflow ~pkgs:[package] ~descr:"samtools.indexed_bam_of_sam" [
    mkdir_p dest ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> dest // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      dest // "temp.bam" ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
    rm_rf (dest // "temp.bam") ;
  ]

let sort ?on:order bam =
  workflow ~pkgs:[package] ~descr:"samtools.sort" [
    samtools "sort" [
      option (fun o -> flag string "-n" (o = `name)) order ;
      dep bam ;
      dest ;
    ] ;
    mv (seq [dest ; string ".bam"]) dest ;
  ]

let indexed_bam_of_bam bam =
  workflow  ~pkgs:[package] ~descr:"samtools.indexed_bam_of_bam" [
    mkdir_p dest ;
    samtools "sort" [
      dep bam ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
  ]

let indexed_bam_to_bam =
  selector ["reads.bam"]
