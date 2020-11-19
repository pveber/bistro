open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"bowtie2" ~tag:"2.3.3" () ]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie2_build ?large_index ?noauto ?packed ?bmax ?bmaxdivn ?dcv ?nodc ?noref ?justref ?offrate ?ftabchars ?seed ?cutoff fa =
  Workflow.shell ~descr:"bowtie2_build" ~img ~np:8 ~mem:(Workflow.int (3 * 1024)) [
    mkdir_p dest ;
    cmd "bowtie2-build" [
      option (flag string "--large-index") large_index ;
      option (flag string "--no-auto") noauto ;
      option (flag string "--packed") packed ;
      option (flag string "--nodc") nodc ;
      option (flag string "--noref") noref ;
      option (flag string "--justref") justref ;
      option (opt "--bmax" int) bmax ;
      option (opt "--bmaxdivn" int) bmaxdivn ;
      option (opt "--dcv" int) dcv ;
      option (opt "--offrate" int) offrate ;
      option (opt "--ftabchars" int) ftabchars ;
      opt "--threads" Fn.id np ;
      option (opt "--seed" int) seed ;
      option (opt "--cutoff" int) cutoff ;
      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let flag_of_preset mode preset =
  let flag = match preset with
    | `very_fast -> "--very-fast"
    | `fast -> "--fast"
    | `sensitive -> "--sensitive"
    | `very_sensitive -> "--very-sensitive"
  in
  match mode with
  | `local -> flag ^ "-local"
  | _ -> flag

let flag_of_mode = function
  | `end_to_end -> "--end-to-end"
  | `local -> "--local"

let flag_of_orientation = function
  | `fr -> "--fr"
  | `rf -> "--rf"
  | `ff -> "--ff"

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie2
    ?skip ?qupto ?trim5 ?trim3 ?preset
    ?_N ?_L ?ignore_quals ?(mode = `end_to_end)
    ?a ?k ?_D ?_R ?minins ?maxins ?orientation
    ?no_mixed ?no_discordant ?dovetail ?no_contain ?no_overlap
    ?no_unal ?seed
    ?fastq_format ?(additional_samples = []) index fq_sample =
  let fq_samples = fq_sample :: additional_samples in
  let args = Bowtie.fastq_args `V2 fq_samples in
  Workflow.shell ~descr:"bowtie2" ~img ~mem:(Workflow.int (3 * 1024)) ~np:8 [
    cmd "bowtie2" [
      option (opt "--skip" int) skip ;
      option (opt "--qupto" int) qupto ;
      option (opt "--trim5" int) trim5 ;
      option (opt "--trim3" int) trim3 ;
      option ((flag_of_preset mode) % string) preset ;
      option (opt "-N" int) _N ;
      option (opt "-L" int) _L ;
      option (flag string "--ignore-quals") ignore_quals ;
      (flag_of_mode % string) mode ;
      option (flag string "-a") a ;
      option (opt "-k" int) k ;
      option (opt "-D" int) _D ;
      option (opt "-R" int) _R ;
      option (opt "--minins" int) minins ;
      option (opt "--maxins" int) maxins ;
      option (flag_of_orientation % string) orientation ;
      option (flag string "--no-mixed") no_mixed  ;
      option (flag string "--no-discordant") no_discordant  ;
      option (flag string "--dovetail") dovetail ;
      option (flag string "--no-contain") no_contain ;
      option (flag string "--no-overlap") no_overlap ;
      option (flag string "--no-unal") no_unal ;
      opt "--threads" Fn.id np ;
      option (opt "--seed" int) seed ;
      option (opt "-q" (Bowtie.qual_option % string)) fastq_format ;
      opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
      args ;
      opt "-S" Fn.id dest ;
    ]
  ]
