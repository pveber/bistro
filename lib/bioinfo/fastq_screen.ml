open Core_kernel.Std
open Bistro.Std
open Bistro.EDSL
open Defs


let env = docker_image ~account:"pveber" ~name:"fastq-screen" ~tag:"0.1.11" ()

let rec filter_expr res = function
    [] -> string res
  | h :: t ->
    let res = match h with
    | `Not_map -> res ^ "0"
    | `Uniquely -> res ^ "1"
    | `Multi_maps -> res ^ "2"
    | `Maps -> res ^ "3"
    | `Not_map_or_Uniquely -> res ^ "4"
    | `Not_map_or_Multi_maps -> res ^ "5"
    | `Ignore -> res ^ "-"
    in
    filter_expr res t

let top_expr = function
  | `top1 x -> string (string_of_int x)
  | `top2 (x, y) -> string (string_of_int x ^ "," ^ string_of_int y)

let configuration genomes =
  let database_lines = List.map genomes ~f:(fun (name, fa) ->
      let index = Bowtie2.bowtie2_build fa in
      seq ~sep:"\t" [
        string "DATABASE" ;
        string name ;
        dep index // "index"
      ]
    )
  in
  seq ~sep:"\n" database_lines

let fastq_screen ?bowtie2_opts ?filter ?illumina ?nohits ?pass ?subset
    ?tag ?(threads = 1) ?top ?(lightweight = true) fq genomes =
  workflow ~descr:"fastq_screen" ~np:threads ~mem:(3 * 1024) [
    mkdir_p dest ;
    cmd "fastq_screen" ~env [
      string "--aligner bowtie2" ;
      option (opt "--bowtie2" string) bowtie2_opts ;
      option (opt "--filter" (filter_expr "")) filter ;
      option (flag string "--illumina1_3") illumina ;
      option (flag string "--nohits") nohits ;
      option (opt "--pass" int) pass ;
      option (opt "--subset" int) subset ;
      option (flag string "--tag") tag ;
      opt "--threads" ident np ;
      option (opt "--top" top_expr) top ;
      dep fq ;
      string "--conf" ; file_dump (configuration genomes) ;
      opt "--outdir" ident dest ;
    ] ;
    if lightweight then rm_rf ( dest // "*.fastq" )
    else cmd "" [] ;
    mv ( dest // "*_screen.html"  ) ( dest // "report_screen.html") ;
  ]

let html_report = selector [ "report_screen.html" ]
