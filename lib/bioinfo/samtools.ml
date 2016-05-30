open Core_kernel.Std
open Types
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"samtools" ~tag:"1.3.1" ()

let samtools subcmd args =
  cmd "samtools" ~env (string subcmd :: args)

let sam_of_bam bam =
  workflow ~descr:"samtools.sam_of_bam" [
    samtools "view" [
      opt "-o" ident dest ;
      dep bam ;
    ]
  ]

let indexed_bam_of_sam sam =
  workflow ~descr:"samtools.indexed_bam_of_sam" [
    mkdir_p dest ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> dest // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      dest // "temp.bam" ;
      opt "-o" ident (dest // "reads.bam") ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
    rm_rf (dest // "temp.bam") ;
  ]

let sort ?on:order bam =
  workflow ~descr:"samtools.sort" [
    samtools "sort" [
      option (fun o -> flag string "-n" (o = `name)) order ;
      dep bam ;
      dest ;
    ] ;
    mv (seq [dest ; string ".bam"]) dest ;
  ]

let indexed_bam_of_bam bam =
  workflow ~descr:"samtools.indexed_bam_of_bam" [
    mkdir_p dest ;
    samtools "sort" [
      dep bam ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
  ]

let indexed_bam_to_bam =
  selector ["reads.bam"]
