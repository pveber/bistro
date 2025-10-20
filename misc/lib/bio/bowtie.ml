open Core
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"bowtie" ~tag:"1.1.2" () ]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie_build ?packed ?color fa =
  Workflow.shell ~descr:"bowtie_build" ~img ~mem:(Workflow.int (3 * 1024)) [
    mkdir_p dest ;
    cmd "bowtie-build" [
      option (flag string "-a -p") packed ;
      option (flag string "--color") color ;
      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let fastq_args version fq_samples =
  let (fqs, fqs1, fqs2), (fqs_gz, fqs1_gz, fqs2_gz) =
    Fastq_sample.explode fq_samples
  in
  let fqs = List.map fqs ~f:dep @ List.map fqs_gz ~f:Bistro_unix.Cmd.gzdep in
  let fqs1 = List.map fqs1 ~f:dep @ List.map fqs1_gz ~f:Bistro_unix.Cmd.gzdep in
  let fqs2 = List.map fqs2 ~f:dep @ List.map fqs2_gz ~f:Bistro_unix.Cmd.gzdep in
  let single_end_option = match version with
    | `V1 -> ""
    | `V2 -> "-U"
  in
  List.filter_opt [
    if List.is_empty fqs then None else Some (opt single_end_option (seq ~sep:",") fqs) ;
    if List.is_empty fqs1 then None else Some (
        seq [
          opt "-1" (seq ~sep:",") fqs1 ;
          string " " ;
          opt "-2" (seq ~sep:",") fqs2
        ])
  ]
  |> seq ~sep:" "

let qual_option = function
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq. Phred64 -> "--phred64-quals"

let bowtie ?l ?e ?m ?fastq_format ?n ?v ?maxins ?(additional_samples = []) index fq_sample =
  let fq_samples = fq_sample :: additional_samples in
  let fq_args = fastq_args `V1 fq_samples in
  Workflow.shell ~descr:"bowtie" ~img ~mem:(Workflow.int (3 * 1024)) ~np:8 [
    cmd "bowtie" [
      string "-S" ;
      option (opt "-n" int) n ;
      option (opt "-l" int) l ;
      option (opt "-e" int) e ;
      option (opt "-m" int) m ;
      option (opt "-v" int) v ;
      option (opt "-q" (qual_option % string)) fastq_format ;
      opt "-p" Fn.id np ;
      option (opt "--maxins" int) maxins ;
      seq [dep index ; string "/index"] ;
      fq_args ;
      dest ;
    ]
  ]
