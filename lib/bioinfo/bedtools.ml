open Core_kernel.Std
open Defs
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"bedtools" ~tag:"2.21.0" ()

let bedtools ?stdout subcmd args =
  cmd "bedtools" ?stdout ~env (string subcmd :: args)

type 'a input = Bed | Gff

let bed = Bed
let gff = Gff

module Cmd = struct
  let slop_args ?strand ?header ~mode = [
    option (flag string "-s") strand ;
    option (flag string "-header") header ;
    seq (
      match mode with
      | `both n      -> [ opt "-b" int n ]
      | `left n      -> [ opt "-l" int n ]
      | `right n     -> [ opt "-r" int n ]
      | `left_pct p  -> [ opt "-l" float p ; string "-pct" ]
      | `right_pct p -> [ opt "-l" float p ; string "-pct" ]
      | `both_pct p  -> [ opt "-b" float p ; string "-pct" ]
    )
  ]

  let slop ?strand ?header ~mode format input chrom_size =
    bedtools "slop" ~stdout:dest [
      seq (slop_args ?strand ?header ~mode) ;
      opt "-i" dep input ;
      opt "-g" dep chrom_size ;
    ]
end

let slop ?strand ?header ~mode format input chrom_size =
  workflow ~descr:"bedtools.slop" [
    Cmd.slop ?strand ?header ~mode format input chrom_size
  ]

let intersect ?ubam ?wa ?wb ?loj ?wo ?wao ?u ?c ?v ?f ?_F ?r ?e ?s ?_S
    ?split ?sorted ?g ?header ?filenames ?sortout file files =
    workflow ~descr:"bedtools.intersect" ~mem:(3 * 1024) ~np:8 [
      cmd "bedtools intersect" ~env ~stdout:dest [
        option (flag string "-ubam") ubam ;
        option (flag string "-wa") wa ;
        option (flag string "-wb") wb ;
        option (flag string "-loj") loj ;
        option (flag string "-wo") wo ;
        option (flag string "-wao") wao ;
        option (flag string "-u") u ;
        option (flag string "-c") c ;
        option (flag string "-v") v ;
        option (opt "-f" float) f ;
        option (opt "-F" float) _F ;
        option (flag string "-r") r ;
        option (flag string "-e") e ;
        option (flag string "-s") s ;
        option (flag string "-S") _S ;
        option (flag string "-split") split ;
        option (flag string "-sorted") sorted ;
        option (opt "-g" dep) g ;
        option (flag string "-header") header ;
        option (flag string "-filenames") filenames ;
        option (flag string "-sortout") sortout ;
        opt "-a" dep file ;
        opt "-b" (list dep ~sep:" ") files ;
      ]
    ]

let bamtobed ?bed12 ?split ?splitD ?ed ?tag ?cigar bam =
  workflow ~descr:"bedtools.bamtobed" ~mem:(3 * 1024) ~np:8 [
    cmd "bedtools bamtobed" ~stdout:dest ~env [
      option (flag string "-bed12") bed12 ;
      option (flag string "-split") split ;
      option (flag string "-splitD") splitD ;
      option (flag string "-ed") ed ;
      option (flag string "-tag") tag ;
      option (flag string "-cigar") cigar ;
      opt "-i" dep bam ;
    ]
  ]
