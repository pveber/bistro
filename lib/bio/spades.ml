open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"spades" ~tag:"3.14.0" () ]

module Yaml = struct
  module T = Template_dsl
  let string x = T.quote ~using:'"' (T.string x)
  let list xs = T.seq ~sep:" " [ T.string "[" ; T.seq ~sep:"," xs ; T.string "]" ]
  let obj dict =
    T.seq ~sep:" " [
      T.string "{" ;
      List.map dict ~f:(fun (k, v) -> T.seq ~sep:" " [ T.string k ; T.string ":" ; v ])
      |> T.seq ~sep:"," ;
      T.string "}"
    ]
end

let string_of_orientation = function
  | `rf -> "rf"
  | `fr -> "fr"

let strandness x = string (string_of_orientation x)

(* spades insists on reading file extensions~ *)
let alias se_or_pe compression i =
  let se_or_pe_suffix = match se_or_pe with
    | `SE -> ""
    | `PE_1 -> "_1"
    | `PE_2 -> "_2"
  in
  let compression_suffix = match compression with
    | `Plain -> ""
    | `Compressed -> ".gz"
  in
  seq ~sep:"/" [ tmp ; string (sprintf "sample_%d%s.fq%s" i se_or_pe_suffix compression_suffix) ]

let yaml_se_entry i compression =
  Yaml.(
    obj [
      "type", string "single" ;
      "single reads", list [ alias `SE compression i ] ;
    ]
  )

let yaml_pe_entry ?orientation compression i =
  let maybe_add_orientation xs =
    match orientation with
    | None -> xs
    | Some o -> xs @ [ "orientation", string (string_of_orientation o) ]
  in
  Yaml.(
    [
      "type", string "paired-end" ;
      "left reads", list [ alias `PE_1 compression i ] ;
      "right reads", list [ alias `PE_2 compression i ] ;
    ]
    |> maybe_add_orientation
    |> Yaml.obj
  )


let yaml_entry ?orientation i fq_sample =
  match (fq_sample : Fastq_sample.t) with
  | Fq (Single_end _) -> yaml_se_entry i `Plain
  | Fq_gz (Single_end _) -> yaml_se_entry i `Compressed
  | Fq (Paired_end _) -> yaml_pe_entry ?orientation `Plain i
  | Fq_gz (Paired_end _) -> yaml_pe_entry ?orientation `Compressed i

let yaml_file ?orientation fq_samples =
  List.mapi fq_samples ~f:(yaml_entry ?orientation)
  |> Yaml.list

let renaming_command_of_fastq_sample i fq_sample =
  let ln src dst =
    cmd "ln" [ string "-s" ; dep src ; dst ]
  in
  match (fq_sample : Fastq_sample.t) with
  | Fq (Single_end fq) -> [ ln fq (alias `SE `Plain i) ]
  | Fq_gz (Single_end fq_gz) -> [ ln fq_gz (alias `SE `Compressed i) ]
  | Fq (Paired_end (fq1, fq2)) -> [
      ln fq1 (alias `PE_1 `Plain i) ;
      ln fq2 (alias `PE_2 `Plain i) ;
    ]
  | Fq_gz (Paired_end (fq1_gz, fq2_gz)) -> [
      ln fq1_gz (alias `PE_1 `Compressed i) ;
      ln fq2_gz (alias `PE_2 `Compressed i) ;
    ]

let renaming_commands_of_fastq_samples fq_samples =
  List.concat_mapi fq_samples ~f:renaming_command_of_fastq_sample

let spades
    ?single_cell ?iontorrent
    ?(threads = 4)
    ?(memory = 10)
    samples
  =
  let yaml_file = yaml_file samples in
  let ln_commands = renaming_commands_of_fastq_samples samples in
  Workflow.shell ~np:threads ~img ~mem:(Workflow.int (memory * 1024)) ~descr:"spades" [
      mkdir_p tmp ;
      mkdir_p dest ;
      and_list (
          ln_commands @ [
            cmd "spades.py" [
                option (flag string "--sc") single_cell ;
                option (flag string "--iontorrent") iontorrent ;
                opt "--threads" Fn.id np ;
                opt "--memory" Fn.id (seq [ string "$((" ; mem ; string " / 1024))" ]) ;
                opt "--dataset" Fn.id (file_dump yaml_file) ;
                opt "-o" Fn.id dest ;
              ]
          ]
        )
    ]

let contigs x = Workflow.select x ["contigs.fasta"]
let scaffolds x = Workflow.select x ["scaffolds.fasta"]

let rnaspades ?(threads = 4) ?(memory = 10) ?ss samples =
  let yaml_file = yaml_file ?orientation:ss samples in
  let ln_commands = renaming_commands_of_fastq_samples samples in
  Workflow.shell ~np:threads ~img ~mem:(Workflow.int (memory * 1024)) ~descr:"rnaspades" [
      mkdir_p tmp ;
      mkdir_p dest ;
      and_list (
          ln_commands @ [
            cmd "rnaspades.py" [
                opt "--threads" Fn.id np ;
                opt "--memory" Fn.id (seq [ string "$((" ; mem ; string " / 1024))" ]) ;
                option (opt "--ss" strandness) ss ;
                opt "--dataset" Fn.id (file_dump yaml_file) ;
                opt "-o" Fn.id dest ;
              ]
          ]
        )
    ]

let transcripts x = Workflow.select x ["transcripts.fasta"]
let hard_filtered_transcripts x = Workflow.select x ["hard_filtered_transcripts.fasta"]
let soft_filtered_transcripts x = Workflow.select x ["soft_filtered_transcripts.fasta"]
