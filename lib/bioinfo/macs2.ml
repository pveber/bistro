open Core_kernel.Std
open Bistro
open Bistro.Std
open Bistro.EDSL_sh
open Types

let package_script = Unix_tools.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/macs2-install/2.1.0.20140616/macs2-install.sh"

let package = {
  pkg_name = "macs2" ;
  pkg_version = "2.1.0" ;
}

let macs2 subcmd opts =
  cmd "macs2" (string subcmd :: opts)

let pileup ?extsize ?both_direction bam =
  workflow ~descr:"macs2.pileup" ~pkgs:[package] [
    macs2 "pileup" [
      opt "-i" dep bam ;
      opt "-o" ident dest ;
      option (flag string "-B") both_direction ;
      option (opt "--extsize" int) extsize ;
    ]
  ]


type gsize = [`hs | `mm | `ce | `dm | `gsize of int]

let gsize_expr = function
  | `hs -> string "hs"
  | `mm -> string "mm"
  | `dm -> string "dm"
  | `ce -> string "ce"
  | `gsize n -> int n

let name = "macs2"

let callpeak ?pvalue ?qvalue ?gsize ?call_summits
             ?fix_bimodal ?extsize ?control treatment =
  workflow ~pkgs:[package] ~descr:"macs2.callpeak" [
    macs2 "callpeak" [
      opt "--outdir" ident dest ;
      opt "--name" string name ;
      opt "--format" string "BAM" ;
      option (opt "--pvalue" float) pvalue ;
      option (opt "--qvalue" float) qvalue ;
      option (opt "--gsize" gsize_expr) gsize ;
      string "--bdg" ;
      option (flag string "--call-summits") call_summits ;
      option (opt "--extsize" int) extsize ;
      option (flag string "--fix-bimodal") fix_bimodal ;
      option (opt "--control" dep) control ;
      opt "--treatment" dep treatment ;
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
