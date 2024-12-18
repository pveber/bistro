(**
   Article: https://journals.plos.org/plosgenetics/article?id=10.1371/journal.pgen.1005870
   Dataset: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE76159
*)

open Base
open Bistro
open Bistro_bio
open Bistro_bio.Formats

let samples = [
  `WT_BHI_1 ; `WT_BHI_2 ; `WT_BHI_3 ;
  `CodY_BHI_1 ; `CodY_BHI_2 ; `CodY_BHI_3 ;
]

let srr_id = function
  | `WT_BHI_1 -> "SRR3033158"
  | `WT_BHI_2 -> "SRR3033159"
  | `WT_BHI_3 -> "SRR3033160"
  | `CodY_BHI_1 -> "SRR3033161"
  | `CodY_BHI_2 -> "SRR3033162"
  | `CodY_BHI_3 -> "SRR3033163"

let genotype = function
  | `WT_BHI_1
  | `WT_BHI_2
  | `WT_BHI_3 -> "WT"
  | `CodY_BHI_1
  | `CodY_BHI_2
  | `CodY_BHI_3 -> "deltaCodY"

let fastq x =
  Sra_toolkit.(fastq_dump fastq (`id (srr_id x)))

let genome : fasta file =
  Bistro_unix.wget "ftp://ftp.ensemblgenomes.org/pub/bacteria/release-41/fasta/bacteria_21_collection/listeria_monocytogenes_10403s/dna/Listeria_monocytogenes_10403s.ASM16869v2.dna_rm.chromosome.Chromosome.fa.gz"
  |> Bistro_unix.gunzip

let bowtie2_index = Bowtie2.bowtie2_build genome

let mapped_reads x =
  Bowtie2.bowtie2 bowtie2_index (Fastq_sample.Fq (Single_end (fastq x)))

let annotation : gff file =
  Bistro_unix.wget "ftp://ftp.ensemblgenomes.org/pub/bacteria/release-41/gff3/bacteria_21_collection/listeria_monocytogenes_10403s/Listeria_monocytogenes_10403s.ASM16869v2.41.chromosome.Chromosome.gff3.gz"
  |> Bistro_unix.gunzip

let counts x =
  Subread.featureCounts
    ~feature_type:"gene"
    ~attribute_type:"gene_id"
    ~strandness:`Unstranded
    ~q:5
    annotation
    (mapped_reads x)
  |> Subread.featureCounts_htseq_tsv

let differential_analysis =
  DESeq2.main_effects
    ["genotype"]
    (List.map samples ~f:(fun x -> [genotype x], counts x))

let () =
  let open Bistro_utils.Repo in
  [
    item ["delme"] (counts `WT_BHI_3) ;
    item ["deseq2"] differential_analysis#directory ;
  ]
  |> build_main ~np:4 ~mem:(`GB 4) ~outdir:"res" ~loggers:[Bistro_utils.Console_logger.create ()]
