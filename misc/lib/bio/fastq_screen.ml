open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"fastq-screen" ~tag:"0.14.0" () ]

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
  | `top1 x -> string (Int.to_string x)
  | `top2 (x, y) -> string (Int.to_string x ^ "," ^ Int.to_string y)

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
    ?tag ?(threads = 1) ?top ?(lightweight = true) (fq_sample : Fastq_sample.t) genomes =
  let se_or_pe_links : (template * template) SE_or_PE.t =
    match fq_sample with
    | Fq (Single_end fq) -> Single_end (dep fq, tmp // "sample.fq")
    | Fq_gz (Single_end fq_gz) -> Single_end (dep fq_gz, tmp // "sample.fq.gz")
    | Fq (Paired_end (fq1, fq2)) -> Paired_end (
        (dep fq1, tmp // "sample_r1.fq"),
        (dep fq2, tmp // "sample_r2.fq")
      )
    | Fq_gz (Paired_end (fq1_gz, fq2_gz)) -> Paired_end (
        (dep fq1_gz, tmp // "sample_r1.fq.gz"),
        (dep fq2_gz, tmp // "sample_r2.fq.gz")
      )
  in
  let fq_screen_args = match se_or_pe_links with
    | Single_end (_, fq_link) -> fq_link
    | Paired_end ((_, fq_link1), (_, fq_link2)) ->
      seq ~sep:" " [ fq_link1 ; fq_link2 ]
  in
  let ln_cmd = match se_or_pe_links with
    | Single_end (fq, fq_link) -> [ cmd "ln -s" [ fq ; fq_link ] ]
    | Paired_end ((fq1, fq_link1), (fq2, fq_link2)) -> [
        cmd "ln -s" [ fq1 ; fq_link1 ] ;
        cmd "ln -s" [ fq2 ; fq_link2 ] ;
      ]
  in
  Workflow.shell ~descr:"fastq_screen" ~img ~np:threads ~mem:(Workflow.int (3 * 1024)) [
      mkdir_p dest ;
      pipe (
          ln_cmd
          @
            [
              cmd "fastq_screen" [
                  string "--aligner bowtie2" ;
                  option (opt "--bowtie2" string) bowtie2_opts ;
                  option (opt "--filter" (filter_expr "")) filter ;
                  option (flag string "--illumina1_3") illumina ;
                  option (flag string "--nohits") nohits ;
                  option (opt "--pass" int) pass ;
                  option (opt "--subset" int) subset ;
                  option (flag string "--tag") tag ;
                  opt "--threads" Fn.id np ;
                  option (opt "--top" top_expr) top ;
                  fq_screen_args ;
                  string "--conf" ; file_dump (configuration genomes) ;
                  opt "--outdir" Fn.id dest ;
                ]
            ]
        ) ;
      if lightweight then rm_rf ( dest // "*.fastq" )
      else cmd "" [] ;
      mv ( dest // "*_screen.html"  ) ( dest // "report_screen.html") ;
    ]

let html_report x = Workflow.select x [ "report_screen.html" ]
