open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"bedtools" ~tag:"2.21.0" () ]

let bedtools ?stdout subcmd args =
  cmd "bedtools" ?stdout (string subcmd :: args)

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

  let slop ?strand ?header ~mode input chrom_size =
    bedtools "slop" ~stdout:dest [
      seq (slop_args ?strand ?header ~mode) ;
      opt "-i" dep input ;
      opt "-g" dep chrom_size ;
    ]
end

let slop ?strand ?header ~mode _ input chrom_size =
  Workflow.shell ~descr:"bedtools.slop" ~img [
    Cmd.slop ?strand ?header ~mode input chrom_size
  ]

let intersect ?ubam ?wa ?wb ?loj ?wo ?wao ?u ?c ?v ?f ?_F ?r ?e ?s ?_S
    ?split ?sorted ?g ?header ?filenames ?sortout _ file files =
  Workflow.shell ~descr:"bedtools.intersect" ~img [
    cmd "bedtools intersect" ~stdout:dest [
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

let closest ?strand ?io ?iu ?id ?fu ?fd ?ties ?mdb ?k ?header _ query beds =
  Workflow.shell ~descr:"bedtools.intersect" ~img [
    cmd "bedtools.closest" ~stdout:dest [
      option ((function `same -> "-s" | `opposite -> "-S") % string) strand ;
      option (flag string "-io") io ;
      option (flag string "-iu") iu ;
      option (flag string "-id") id ;
      option (flag string "-fu") fu ;
      option (flag string "-fd") fd ;
      option (opt "-t" ((function `all -> "all" | `first -> "first" | `last -> "last") % string)) ties ;
      option (opt "-mdb" ((function `each -> "each" | `all -> "all") % string)) mdb ;
      option (opt "-k" int) k ;
      option (flag string "-header") header ;
      opt "-a" dep query ;
      opt "-b" (list dep ~sep:" ") beds ;
    ]
  ]

let bamtobed ?bed12 ?split ?splitD ?ed ?tag ?cigar bam =
  Workflow.shell ~descr:"bedtools.bamtobed" ~img ~mem:(Workflow.int  (3 * 1024)) ~np:8 [
    cmd "bedtools bamtobed" ~stdout:dest [
      option (flag string "-bed12") bed12 ;
      option (flag string "-split") split ;
      option (flag string "-splitD") splitD ;
      option (flag string "-ed") ed ;
      option (flag string "-tag") tag ;
      option (flag string "-cigar") cigar ;
      opt "-i" dep bam ;
    ]
  ]


let strand_arg x =
  string (
    match x with
    | `plus -> "+"
    | `minus -> "-"
  )

let operation_arg x =
  string (
    match x with
    | `sum -> "sum"
    | `min -> "min"
    | `max -> "max"
    | `absmin -> "absmin"
    | `mean -> "mean"
    | `median -> "median"
    | `collapse -> "collapse"
    | `distinct -> "distinct"
    | `count -> "count"
    | `count_distinct -> "count_distinct"
  )

let concat_beds_dep = function
  | [] -> string ""
  | xs ->
    seq ~sep:"" [
      string "<(cat " ;
      list ~sep:" " dep xs ;
      string "| sort -k1,1 -k2,2n)"
    ]

let merge ?s ?_S ?d ?c ?o beds =
  Workflow.shell ~descr:"bedtools.merge" ~img [
    cmd "bedtools" ~stdout:dest [
      string "merge" ;
      option (flag string "-s") s ;
      option (opt "-S" strand_arg) _S ;
      option (opt "-d" int) d ;
      option (opt "-c" (list ~sep:"," int)) c ;
      option (opt "-o" (list ~sep:"," operation_arg)) o ;
      opt "-i" concat_beds_dep beds ;
    ]
  ]
