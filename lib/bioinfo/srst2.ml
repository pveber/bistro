open Core_kernel.Std
open Bistro.EDSL


let env = docker_image ~account:"pveber" ~name:"srst2" ~tag:"0.2.0" ()


let run_gen_cmd ?mlst_db ?mlst_delimiter ?mlst_definitions
    ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
    ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
    ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
    ?baseq ?samtools_args ?report_new_consensus
    ?report_all_consensus cmd_name other_args =
  cmd cmd_name ~env (
    List.append [
        option (opt "--mlst_db" dep) mlst_db ;
        option (opt "--mlst_delimiter" string) mlst_delimiter ;
        option (opt "--mlst_definitions" dep) mlst_definitions ;
        option (opt "--mlst_max_mismatch" int) mlst_max_mismatch ;
        option (opt "--gene_db" (list ~sep:" " dep)) gene_db ;
        option (flag string "--no_gene_details") no_gene_details ;
        option (opt "--gene_max_mismatch" int) gene_max_mismatch ;
        option (opt "--min_coverage" int) min_coverage ;
        option (opt "--max_divergence" int) max_divergence ;
        option (opt "--min_depth" int) min_depth ;
        option (opt "--min_edge_depth" int) min_edge_depth ;
        option (opt "--prob_err" float) prob_err ;
        option (opt "--truncation_score_tolerance" int) truncation_score_tolerance ;
        option (opt "--other" string) other ;
        option (opt "--max_unaligned_overlap" int) max_unaligned_overlap ;
        option (opt "--mapq" int) mapq ;
        option (opt "--baseq" int) baseq ;
        option (opt "--samtools_args" string) samtools_args ;
        option (flag string "--report_new_consensus") report_new_consensus ;
        option (flag string "--report_all_consensus") report_all_consensus ;
      ]
      other_args
  )

let run_se ?mlst_db ?mlst_delimiter ?mlst_definitions
    ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
    ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
    ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
    ?baseq ?samtools_args ?report_new_consensus
    ?report_all_consensus ?(threads = 1) fq =
  workflow ~descr:"srst2" ~np:threads ~mem:(3 * 1024) [
    mkdir_p dest ;
    run_gen_cmd "srst2" ?mlst_db ?mlst_delimiter ?mlst_definitions
    ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
    ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
    ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
    ?baseq ?samtools_args ?report_new_consensus
    ?report_all_consensus [
      opt "--threads" ident np ;
      opt "--input_se" (list ~sep:" " dep) fq ;
      opt "--output" ident dest ;
    ] ;
  ]

let run_pe ?mlst_db ?mlst_delimiter ?mlst_definitions
    ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
    ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
    ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
    ?baseq ?samtools_args ?report_new_consensus
    ?report_all_consensus ?(threads = 1) fq =
  workflow ~descr:"srst2" ~np:threads ~mem:(3 * 1024) [
    mkdir_p dest ;
    run_gen_cmd "srst2" ?mlst_db ?mlst_delimiter ?mlst_definitions
    ?mlst_max_mismatch ?gene_db ?no_gene_details ?gene_max_mismatch
    ?min_coverage ?max_divergence ?min_depth ?min_edge_depth ?prob_err
    ?truncation_score_tolerance ?other ?max_unaligned_overlap ?mapq
    ?baseq ?samtools_args ?report_new_consensus
    ?report_all_consensus [
      opt "--threads" ident np ;
      opt "--input_pe" (list ~sep:" " dep) fq ;
      opt "--output" ident dest ;
    ] ;
  ]
