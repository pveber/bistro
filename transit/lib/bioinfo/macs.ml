open Core_kernel
open Bistro.EDSL
open Defs

let env = docker_image ~account:"pveber" ~name:"macs" ~tag:"1.4.2" ()


type _ format =
  | Sam
  | Bam

let sam = Sam
let bam = Bam

let opt_of_format = function
  | Sam -> "SAM"
  | Bam -> "BAM"

type gsize = [ `hs | `mm | `ce | `dm | `gsize of int ]

let gsize_expr = function
  | `hs -> string "hs"
  | `mm -> string "mm"
  | `dm -> string "dm"
  | `ce -> string "ce"
  | `gsize n -> int n

type keep_dup = [ `all | `auto | `int of int ]

let keep_dup_expr = function
  | `all -> string "all"
  | `auto -> string "auto"
  | `int n -> int n

let name = "macs"

let run ?control ?petdist ?gsize ?tsize ?bw ?pvalue ?mfold ?nolambda
    ?slocal ?llocal ?on_auto ?nomodel ?shiftsize ?keep_dup
    ?to_large ?wig ?bdg ?single_profile ?space ?call_subpeaks
    ?diag ?fe_min ?fe_max ?fe_step format treatment =
  workflow ~descr:"macs" ~mem:(3 * 1024) ~np:8  [
    mkdir_p dest ;
    cmd "macs14" ~env [
      option (opt "--control" (list ~sep:"," dep)) control ;
      opt "--name" seq [ dest ; string "/" ; string name ] ;
      opt "--format" (fun x -> x |> opt_of_format |> string) format ;
      option (opt "--petdist" int) petdist ;
      option (opt "--gsize" gsize_expr) gsize ;
      option (opt "--tsize" int) tsize ;
      option (opt "--bw" int) bw ;
      option (opt "--pvalue" float) pvalue ;
      option (opt "--mfold" (fun (i, j) -> seq ~sep:"," [int i ; int j])) mfold ;
      option (flag string "--nolambda") nolambda ;
      option (opt "--slocal" int) slocal ;
      option (opt "--llocal" int) llocal ;
      option (flag string "--on-auto") on_auto ;
      option (flag string "--nomodel") nomodel ;
      option (opt "--shiftsize" int) shiftsize ;
      option (opt "--keep-dup" keep_dup_expr) keep_dup ;
      option (flag string "--to-large") to_large ;
      option (flag string "--wig") wig ;
      option (flag string "--bdg") bdg ;
      option (flag string "--single-profile") single_profile ;
      option (opt "--space" int) space ;
      option (flag string "--call-subpeaks") call_subpeaks ;
      option (flag string "--diag") diag ;
      option (opt "--fe-min" int) fe_min ;
      option (opt "--fe-max" int) fe_max ;
      option (opt "--fe-step" int) fe_step ;
      opt "--treatment" (list ~sep:"," dep) treatment ;
      ident dest ;
    ]
  ]

class type peaks_xls = object
  inherit bed3
  method f4 : int
  method f5 : int
  method f6 : int
  method f7 : float
  method f8 : float
  method f9 : float
end

let peaks_xls = selector [ name ^ "_peaks.xls" ]

class type narrow_peaks = object
  inherit bed5
  method f6 : string
  method f7 : float
  method f8 : float
  method f9 : float
  method f10 : int
end

let narrow_peaks =
  selector [ name ^ "_peaks.narrowPeak" ]

class type peak_summits = object
  inherit bed4
  method f5 : float
end

let peak_summits = selector [ name ^ "_summits.bed" ]
