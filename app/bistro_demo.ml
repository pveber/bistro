(** {:{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506}GEO Series GSE29506} *)

open Core.Std
open Bistro.Std
open Bistro.EDSL
open Bistro_bioinfo.Std

let common_spec =
  let open Command.Spec in
  empty
  +> flag "--tmpdir"  (optional string) ~doc:"DIR (Preferably local) directory where to put temporary files"
  +> flag "--outdir"  (required string) ~doc:"DIR Directory where to link exported targets"
  +> flag "--np"      (optional_with_default 4 int) ~doc:"INT Number of processors"
  +> flag "--mem"     (optional_with_default 4 int) ~doc:"INT Available memory (in GB)"

module ChIP_seq = struct
  let chIP_pho4_noPi = List.map ~f:Sra.fetch_srr [ "SRR217304" ; "SRR217305" ]

  let genome = Ucsc_gb.genome_sequence `sacCer2

  (* SAMPLES AS FASTQ *)
  let chIP_pho4_noPi_fq = List.map chIP_pho4_noPi ~f:Sra_toolkit.fastq_dump

  (* MAPPING *)
  let bowtie_index = Bowtie.bowtie_build genome
  let chIP_pho4_noPi_sam = Bowtie.bowtie ~v:2 bowtie_index (`single_end chIP_pho4_noPi_fq)
  let chIP_pho4_noPi_bam = Samtools.(indexed_bam_of_sam chIP_pho4_noPi_sam / indexed_bam_to_bam)

  let chIP_pho4_noPi_macs2 = Macs2.callpeak ~mfold:(1,100) chIP_pho4_noPi_bam

  let main tmpdir outdir np mem () =
    Bistro_app.(
      local  ~use_docker:true ?tmpdir ~np ~mem:(mem * 1024) ~outdir [
        [ "chIP_pho4_noPi_macs2.peaks" ] %> chIP_pho4_noPi_macs2
      ]
    )

  let spec = common_spec

  let command =
    Command.basic
      ~summary:"Analysis of a ChIP-seq dataset"
      spec
      main
end

module RNA_seq = struct
  let samples = [
    (`WT, `High_Pi) ;
    (`WT, `No_Pi 15) ;
  ]

  let sra_id = function
    | `WT, `High_Pi -> "SRR1583715"
    | `WT, `No_Pi 15 -> "SRR1583716"
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
      [ [  "0" ], counts (`WT, `High_Pi) ;
        [ "15" ], counts (`WT, `No_Pi 15) ; ]

  let main tmpdir outdir np mem () =
    Bistro_app.(
      local  ~use_docker:true ?tmpdir ~np ~mem:(mem * 1024) ~outdir [
        [ "deseq2" ] %> deseq2#effect_table ;
      ]
    )

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
