open Core
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

module Make(S : Sample) = struct
  let genome x = match x with
    | Ucsc_gb org -> Ucsc_gb.genome_sequence org
    | Fasta fa -> fa.sequence

  let bowtie2_index x =
    genome x
    |> Bowtie2.bowtie2_build

  let mapped_reads x =
    let List1.Cons (fq, other_fqs) = S.fastq_samples x in
    let fq_samples = fq :: other_fqs in
    let all_single_end = List.for_all fq_samples ~f:Fastq_sample.is_single_end in
    Bowtie2.bowtie2 ~maxins:800
      (bowtie2_index (S.reference_genome x))
      ~no_mixed:true
      ~no_discordant:(not all_single_end)
      ~additional_samples:other_fqs
      fq
    |> Samtools.(view ~output:sam ~h:true ~q:5)


  let mapped_reads_indexed_bam x =
    Samtools.indexed_bam_of_sam (mapped_reads x)

  let mapped_reads_bam x =
    mapped_reads_indexed_bam x |> Samtools.indexed_bam_to_bam

  let mapped_reads_nodup x =
    Picardtools.(
      markduplicates
        ~remove_duplicates:true
        (mapped_reads_indexed_bam x)
      |> reads
    )

  let mapped_reads_nodup_indexed x =
    mapped_reads_nodup x
    |> Samtools.indexed_bam_of_bam

  let coverage x =
    Deeptools.bamcoverage
      ~normalizeUsing:`RPKM
      ~extendreads:100
      Deeptools.bigwig
      (mapped_reads_indexed_bam x)

  let counts ~no_dups ~feature_type ~attribute_type ~gff x =
    Subread.featureCounts
      ~feature_type
      ~attribute_type
      gff
      ((if no_dups then mapped_reads_nodup else mapped_reads_bam) x)

  let fastq_screen ~possible_contaminants x =
    let genomes =
      (match S.reference_genome x with
       | Ucsc_gb org as reference ->
         Ucsc_gb.string_of_genome org, genome reference
       | Fasta fa -> fa.name, fa.sequence)
      :: possible_contaminants
    in
    Fastq_screen.fastq_screen
      ~bowtie2_opts:"--end-to-end"
      ~nohits:true
      (List1.hd (S.fastq_samples x))
      genomes
    |> Fastq_screen.html_report

  let bamstats x =
    Alignment_stats.bamstats (mapped_reads_bam x)

  let bamstats_alt x =
    let f = fun%workflow () ->
      let open Biotk in
      Bam.with_file [%path mapped_reads_bam x] ~f:(fun _ als ->
          Seq.fold_left
            (fun acc r -> Bamstats.update acc (ok_exn r))
            Bamstats.zero als
          |> Result.return
        )
      |> ok_exn
    in
    Workflow.plugin ~descr:"bamstats_alt" f

  let chrstats x =
    Alignment_stats.chrstats (mapped_reads_bam x)

  let alignment_summary =
    Alignment_stats.summary ~sample_name:S.to_string ~mapped_reads:mapped_reads_bam S.all
end
