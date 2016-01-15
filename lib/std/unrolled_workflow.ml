open Types
open Ucsc_gb.Formats
open Experiment_description

type condition = (factor * string) list

module type S = sig

  val project_name : string

  module Genome : sig
    type t = genome

    val list : t list

    val sequence : t -> fasta workflow
    val bowtie_index : t -> Bowtie.index workflow
    val bowtie2_index : t -> Bowtie2.index workflow
  end

  module Model : sig
    type t = model

    val list : t list
    val annotation : t -> annotation option
    val gene_annotation : t -> gff file workflow option
  end

  module sam le : sig
    type t = sample

    val list : t list

    val model : t -> model
    val genome : t -> genome option
    val condition : t -> (factor * string) list

    val ucsc_genome : t -> Ucsc_gb.genome option

    (** Short read samples *)
    val short_read_data : t -> short_read_data option
    val sanger_fastq  : t -> [`sanger] fastq workflow list se_or_pe option
    val fastQC_report : t -> FastQC.workflow se_or_pe option

    (** Short read samples with a reference genome *)
    val mapped_reads : t -> bam workflow option
    val mapped_reads_indexed : t -> [ `indexed_bam ] directory workflow option
    val mapped_reads_sam : t -> sam workflow option

    val signal : t -> bigWig workflow option

    (** Peak calling stuff *)
    val chIP_TF : t -> string option
    val macs2_peak_calling : t -> [`macs2_callpeak_output] directory workflow option
    val peak_calling : t -> Macs2.narrow_peaks workflow option

    (** mRNA-seq analysis *)
    val read_counts_per_gene : t -> Htseq.count_tsv workflow option
  end

  module Condition : sig
    type t = (factor * string) list
    val list : t list
    val pairs : (t * t) list
  end

  module Transcriptome : sig
    (* val deseq2_wrapper_output : Deseq2.wrapper_output workflow option *)
  end
end
