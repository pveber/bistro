(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Core.Std
open Bistro.Std
open Bistro.EDSL
open Bistro_bioinfo.Std
open Bistro_engine

let common_spec =
  let open Command.Spec in
  empty
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"
  +> flag "--verbose" no_arg ~doc:" Logs build events on the console"
  +> flag "--html-report" (optional string) ~doc:"PATH Logs build events in an HTML report"

let logger verbose html_report =
  Bistro_logger.tee
    (if verbose then Bistro_console_logger.create () else Bistro_logger.null)
    (match html_report with
     | Some path -> Bistro_html_logger.create path
     | None -> Bistro_logger.null)


let main repo outdir np mem verbose html_report () =
  let open Bistro_repo in
  build
    ~keep_all:false
    ~np ~mem:(mem * 1024)
    ~logger:(logger verbose html_report)
    ~outdir repo

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

  let repo = Bistro_repo.[
    [ "chIP_pho4_noPi_macs2.peaks" ] %> chIP_pho4_noPi_macs2
  ]

  let spec = common_spec

  let command =
    Command.basic
      ~summary:"Analysis of a ChIP-seq dataset"
      spec
      (main repo)
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

  let repo = Bistro_repo.[
    [ "deseq2" ; "0_vs_360" ] %> deseq2#effect_table ;
  ]

  let spec = common_spec

  let command =
    Command.basic
      ~summary:"Analysis of a RNA-seq dataset"
      spec
      (main repo)

end

let command =
  Command.group
    ~summary:"Demo pipelines for bistro"
    [
      "chipseq", ChIP_seq.command ;
      "rnaseq",  RNA_seq.command ;
    ]

let () = Command.run ~version:"0.1" command
