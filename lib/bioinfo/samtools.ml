open Core_kernel.Std
open Bistro.EDSL

type 'a format = Bam | Sam

let bam = Bam
let sam = Sam


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

let bam_of_sam sam =
  workflow ~descr:"samtools.bam_of_sam" [
    samtools "view" [
      string "-S -b" ;
      opt "-o" ident dest ;
      dep sam ;
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
      opt "-o" Fn.id dest ;
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

(* let output_format_expr = function *)
(*   | Bam -> string "-b" *)
(*   | Sam -> string "" *)

(*
let view ?input_format ?output_format ?_1 ?u ?h ?_H ?c ?_L ?q ?m ?f ?_F ?_B ?s file =
  workflow ~descr:"samtools.view" ~mem:(3 * 1024) ~np:8 [
    cmd "samtools view" ~env [
      option output_format_expr output_format ;
      option (flag string "-1") _1 ;
      option (flag string "-u") u ;
      option (flag string "-h") h ;
      option (flag string "-H") _H ;
      option (flag string "-c") c ;
      option (opt "-L" dep) _L ;
      option (opt "-q" int) q ;
      option (opt "-m" int) m ;
      option (opt "-f" int) f ;
      option (opt "-F" int) _F ;
      option (flag string "-B") _B ;
      option (opt "-s" float) s ;
      dep file ;
      opt "-o" ident dest ;
    ]
  ]
*)
