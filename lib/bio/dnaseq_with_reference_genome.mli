open Bistro
open Biotk
open Formats

type reference_genome =
  | Ucsc_gb of Ucsc_gb.genome
  | Fasta of { name : string ; sequence : fasta file }

module type Sample = sig
  type t
  val reference_genome : t -> reference_genome
  val all : t list
  val to_string : t -> string
  val fastq_samples : t -> Fastq_sample.t List1.t
end

module Make(S : Sample) : sig
  val mapped_reads : S.t -> sam file
  val mapped_reads_indexed_bam : S.t -> [`indexed_bam] directory
  val mapped_reads_bam : S.t -> bam file
  val mapped_reads_nodup : S.t -> bam file
  val mapped_reads_nodup_indexed : S.t -> [`indexed_bam] directory
  val coverage : S.t -> Ucsc_gb.bigWig file
  val counts :
    no_dups:bool ->
    feature_type:string ->
    attribute_type:string ->
    gff:gff file ->
    S.t ->
    [`featureCounts] directory
  val fastq_screen :
    possible_contaminants:(string * fasta file) list ->
    S.t ->
    html file
  val bamstats : S.t -> text file
  val bamstats_alt : S.t -> Biotk.Bamstats.t workflow
  val chrstats : S.t -> text file
  val alignment_summary : html file
end
