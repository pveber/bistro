open Bistro
open Bistro.Shell_dsl

type 'a format = NarrowPeak | BroadPeak | Bed | Gff

let narrowPeak = NarrowPeak
let broadPeak = BroadPeak
let bed = Bed
let gff = Gff

type 'a output = [`idr_output of 'a]

let string_of_file_format = function
  | NarrowPeak -> "narrowPeak"
  | BroadPeak -> "broadPeak"
  | Bed -> "bed"
  | Gff -> "gff"

let file_format x = string (string_of_file_format x)

let string_of_merge_method = function
  | `sum -> "sum"
  | `avg -> "avg"
  | `min -> "min"
  | `max -> "max"

let merge_method x = string (string_of_merge_method x)

let token_of_rank r =
  string (
    match r with
    | `signal -> "signal.value"
    | `pvalue -> "p.value"
    | `qvalue -> "q.value"
  )

let img = [ docker_image ~account:"pveber" ~name:"idr" ~tag:"2.0.3" () ]

let idr
    ~input_file_type ?idr_threshold ?soft_idr_threshold
    ?peak_merge_method ?rank ?random_seed ?peak_list
    sample1 sample2 =
  Workflow.shell ~descr:"Idr.idr" ~img [
    mkdir_p dest ;
    cmd "idr" [
      opt "--input-file-type" file_format input_file_type ;
      opt "--output-file" (fun x -> x) (dest // "items.tsv") ;
      option (opt "--idr-threshold" float) idr_threshold ;
      option (opt "--soft-idr-threshold" float) soft_idr_threshold ;
      option (opt "--peak-merge-method" merge_method) peak_merge_method ;
      option (opt "--rank" token_of_rank) rank ;
      option (opt "--random-seed" int) random_seed ;
      option (opt "--peak-list" dep) peak_list ;
      string "--plot" ;
      opt "--samples" (list ~sep:" " dep) [ sample1 ; sample2 ] ;
    ]
  ]

let items x = Workflow.select x [ "items.tsv" ]
let figure x = Workflow.select x [ "items.tsv.png" ]
