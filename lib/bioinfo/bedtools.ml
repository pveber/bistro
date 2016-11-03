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
