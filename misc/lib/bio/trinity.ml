open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"trinity" ~tag:"2.9.1" () ]

let fqs_option_template fastq_samples =
  let (fqs, fq1s, fq2s), (fq_gzs, fq1_gzs, fq2_gzs) = Fastq_sample.explode fastq_samples in
  let arg_list plain_files compressed_files =
    let tokens = List.map plain_files ~f:dep @ List.map compressed_files ~f:Bistro_unix.Cmd.gzdep in
    match tokens with
    | [] -> None
    | _ :: _ -> Some (seq ~sep:"," tokens)
  in
  let maybe_opt o = Option.map ~f:(opt o Fn.id) in
  [
    maybe_opt "--single" (arg_list fqs fq_gzs) ;
    maybe_opt "--left" (arg_list fq1s fq1_gzs) ;
    maybe_opt "--right" (arg_list fq2s fq2_gzs) ;
  ]
  |> List.filter_opt
  |> seq ~sep:" "

let ss_lib_type_option o =
  opt "--SS_lib_type" string
    (match o with
     | `R -> "R"
     | `F -> "F"
     | `RF -> "RF"
     | `FR -> "FR")

(** https://github.com/trinityrnaseq/trinityrnaseq/wiki/Running-Trinity *)
let trinity ?(mem = 128) ?(threads = 4) ?no_normalize_reads ?run_as_paired
    ?min_kmer_cov ?ss_lib_type
    se_or_pe_fq = 
  let tmp_dest = tmp // "trinity" in
  Workflow.shell ~descr:"trinity" ~img ~np:threads ~mem:(Workflow.int (mem * 1024)) [
    mkdir_p tmp ;
    cmd "Trinity" ~stdout:(string "/dev/null")[
      string "--seqType fq" ;
      fqs_option_template se_or_pe_fq ;
      option (flag string "--no_normalize_reads") no_normalize_reads ;
      option (flag string "--run_as_paired") run_as_paired ;
      option (opt "--min_kmer_cov" int) min_kmer_cov ;
      option ss_lib_type_option ss_lib_type ;
      opt "--CPU" Fun.id np ;
      opt "--max_memory" Fun.id (seq [ string "$((" ; Bistro.Shell_dsl.mem ; string " / 1024))G" ]) ;
      opt "--output" Fun.id tmp_dest ;
    ] ;
    cmd "mv" [
      tmp_dest // "Trinity.fasta" ;
      dest ;
    ]
  ]

let prepare_fastq n fq =
  Workflow.shell ~descr:"trinity.prepare_fastq" [
    pipe [
      cmd "zcat" [ dep fq ] ;
      cmd "awk" ~stdout:dest [
        sprintf {|'{ if (NR%%4==1 && !($1 ~ /.*\/%d/)) { print $1"/%d" } else { print } }'|} n n
        |> string
      ] ;
    ]
  ]

let uniq_count_stats sam =
  let sorted_sam =
    sam
    |> Samtools.bam_of_sam
    |> Samtools.sort ~on:`name
  in
  Workflow.shell ~descr:"trinity.uniq_count_stats.pl" ~img [
    cmd "$TRINITY_HOME/util/SAM_nameSorted_to_uniq_count_stats.pl" ~stdout:dest [
      dep sorted_sam ;
    ]
  ]

let fq_option_template = function
  | SE_or_PE.Single_end fqs -> opt "--single" dep fqs
  | Paired_end (fqs1, fqs2) ->
    seq ~sep:" " [
      opt "--left"  dep fqs1 ;
      opt "--right" dep fqs2 ;
    ]

let bash_app f x =
  seq ~sep:"" (string "$(" :: string f :: string " " :: x @ [ string ")" ])

(* https://github.com/trinityrnaseq/trinityrnaseq/wiki/Trinity-Insilico-Normalization#trinitys-in-silico-read-normalization *)
let insilico_read_normalization ?(mem = 128) ?pairs_together ?parallel_stats ~max_cov se_or_pe_fq =
  let trinity_cmd =
    cmd "$TRINITY_HOME/util/insilico_read_normalization.pl" [
      string "--seqType fq" ;
      fq_option_template se_or_pe_fq ;
      opt "--CPU" Fun.id np ;
      opt "--JM" Fun.id (seq [ string "$((" ; Bistro.Shell_dsl.mem ; string " / 1024))G" ]) ;
      opt "--max_cov" int max_cov ;
      option (flag string "--pairs_together") pairs_together ;
      option (flag string "--PARALLEL_STATS") parallel_stats ;
      opt "--output" Fun.id tmp ;
    ]
  in
  let workflow post =
    Workflow.shell
      ~descr:"trinity.insilico_read_normalization"
      ~np:32 ~mem:(Workflow.int (mem * 1024))
      ~img
      (trinity_cmd :: post)
  in
  let mv x y = mv (bash_app "readlink" [ tmp // x ]) y in
  match se_or_pe_fq with
  | Single_end _ ->
    SE_or_PE.Single_end (workflow [ mv "single.norm.fq" dest ])
  | Paired_end _ ->
    let post = [
      mkdir_p dest ;
      mv "left.norm.fq" (dest // "left.fq") ;
      mv "right.norm.fq" (dest // "right.fq") ;
    ]
    in
    let inner = workflow post in
    Paired_end (
      Workflow.select inner ["left.fq"],
      Workflow.select inner ["right.fq"]
    )

let get_Trinity_gene_to_trans_map fa =
  Workflow.shell ~descr:"get_Trinity_gene_to_trans_map" ~img [
    cmd "$TRINITY_HOME/util/support_scripts/get_Trinity_gene_to_trans_map.pl" ~stdout:dest [
      dep fa
    ]
  ]
