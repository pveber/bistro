(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Core.Std
open Bistro.Std
open Bistro.EDSL
open Bistro_bioinfo.Std

let common_spec =
  let open Command.Spec in
  empty
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"+> flag "--verbose" no_arg ~doc:" Logs build events on the console"

let logger verbose =
  if verbose then
    let open Bistro_console_logger in
    Some (event (create ()))
  else
    None

module ChIP_seq = struct
  let chIP_pho4_noPi = List.map ~f:Sra.fetch_srr [ "SRR217304" ; "SRR217305" ]

  let genome = Ucsc_gb.genome_sequence `sacCer2

  (* SAMPLES AS FASTQ *)
  let chIP_pho4_noPi_fq = List.map chIP_pho4_noPi ~f:Sra_toolkit.fastq_dump

  (* MAPPING *)
  let bowtie_index = Bowtie.bowtie_build genome
  let chIP_pho4_noPi_sam = Bowtie.bowtie ~v:2 bowtie_index (`single_end chIP_pho4_noPi_fq)
  let chIP_pho4_noPi_bam = Samtools.(indexed_bam_of_sam chIP_pho4_noPi_sam / indexed_bam_to_bam)

  let chIP_pho4_noPi_macs2 = Macs2.callpeak ~mfold:(1,100) Macs2.bam [ chIP_pho4_noPi_bam ]

  let main outdir np mem verbose () =
    let open Bistro_app in
    let repo = [
      [ "chIP_pho4_noPi_macs2.peaks" ] %> chIP_pho4_noPi_macs2
    ]
    in
    run ~use_docker:true ~np ~mem:(mem * 1024) ?log:(logger verbose) (of_repo ~outdir repo)

  let spec = common_spec

  let command =
    Command.basic
      ~summary:"Analysis of a ChIP-seq dataset"
      spec
      main
end

module RNA_seq = struct
  (* http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE61661 *)

  let samples = [
    (`WT, `High_Pi) ;
    (`WT, `No_Pi 360) ;
  ]

  let sra_id = function
    | `WT, `High_Pi  -> "SRR1583715"
    | `WT, `No_Pi 360 -> "SRR1583740"
    | `WT, `No_Pi _ -> assert false

  let sra x = Sra.fetch_srr (sra_id x)

  let fastq x = Sra_toolkit.fastq_dump (sra x)

  let genome = Ucsc_gb.genome_sequence `sacCer2

  (* MAPPING *)
  let bowtie_index = Bowtie.bowtie_build genome

  let bam x =
    Tophat.(
      tophat1
        bowtie_index
        (`single_end [ fastq x ])
      /
      accepted_hits
    )

  (* oddly the gff from sgd has a fasta file at the end, which htseq-count
     doesn't like. This is a step to remove it. *)
  let remove_fasta_from_gff gff =
    workflow ~descr:"remove_fasta_from_gff" [
      cmd "sed" ~stdout:dest [
        string "'/###/q'" ;
        dep gff ;
      ]
    ]

  let gene_annotation : gff workflow =
    Unix_tools.wget
      "http://downloads.yeastgenome.org/curation/chromosomal_feature/saccharomyces_cerevisiae.gff"
    |> remove_fasta_from_gff

  let counts x =
    Htseq.count
      ~stranded:`no ~feature_type:"gene" ~idattribute:"Name"
      (`bam (bam x)) gene_annotation

  let deseq2 =
    Deseq2.main_effects
      ["time"]
      [ [   "0" ], counts (`WT, `High_Pi) ;
        [ "360" ], counts (`WT, `No_Pi 360) ; ]

  let main outdir np mem verbose () =
    let open Bistro_app in
    let repo = [
      [ "deseq2" ; "0_vs_360" ] %> deseq2#effect_table ;
    ]
    in
    run ~use_docker:true ~np ~mem:(mem * 1024) ?log:(logger verbose) (of_repo ~outdir repo)

  let spec = common_spec

  let command =
    Command.basic
      ~summary:"Analysis of a RNA-seq dataset"
      spec
      main

end

let command =
  Command.group
    ~summary:"Demo pipelines for bistro"
    [
      "chipseq", ChIP_seq.command ;
      "rnaseq",  RNA_seq.command ;
    ]

let () = Command.run ~version:"0.1" command
