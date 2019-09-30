(**
   Paper: https://www.ncbi.nlm.nih.gov/pubmed/21700227
   Datasets: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506
*)
open Core
open Bistro_bioinfo
open Bistro_utils

let np = 4

type chIP_sample = [ `ChIP_Pho4_noPi ]
[@@deriving show, enumerate]


type factor = [ `Pho4 ]
[@@deriving show, enumerate]

let factor = function
  | `ChIP_Pho4_noPi -> `Pho4

let control_sample = function
  | `ChIP_Pho4_noPi -> `Input_WT_NoPi

let genome = Ucsc_gb.genome_sequence `sacCer2
let genome_2bit = Ucsc_gb.genome_2bit_sequence `sacCer2

let srr_id = function
  | `ChIP_Pho4_noPi -> [ "SRR217304" ; "SRR217305" ]
  | `Input_WT_NoPi -> [ "SRR217324" ]

let fastq x = List.map (srr_id x) ~f:(fun id ->
    Sra_toolkit.fastq_dump (`id id)
  )

let bowtie_index = Bowtie.bowtie_build genome

let mapped_reads x =
  Bowtie.bowtie ~v:1 bowtie_index (SE_or_PE.Single_end (fastq x))

let mapped_reads_bam x =
  Samtools.indexed_bam_of_sam (mapped_reads x)

let tf_peaks treatment_sample =
  let control_sample = control_sample treatment_sample in
  let treatment = mapped_reads treatment_sample in
  let control = mapped_reads control_sample in
  Macs2.callpeak ~mfold:(1,100) Macs2.sam ~control:[ control ] [ treatment ]

let peak_sequences ~radius treatment_sample =
  let summits = Macs2.peak_summits (tf_peaks treatment_sample) in
  let chrom_sizes = Ucsc_gb.fetchChromSizes `sacCer2 in
  let regions = Bedtools.(slop ~mode:(`both radius) bed summits chrom_sizes) in
  Ucsc_gb.twoBitToFa genome_2bit (Bed.keep4 regions)

let meme treatment_sample =
  peak_sequences ~radius:50 treatment_sample
  |> Meme_suite.meme ~nmotifs:3 ~minw:5 ~maxw:8 ~revcomp:true ~alphabet:`dna ~maxsize:1_000_000

let meme_chip treatment_sample =
  peak_sequences ~radius:50 treatment_sample
  |> Meme_suite.meme_chip
    ~meme_nmotifs:3 ~meme_minw:5 ~meme_maxw:8

let chipqc =
  let samples = List.map all_of_chIP_sample ~f:(fun x -> {
        ChIPQC.id = show_chIP_sample x ;
        tissue = "yeast" ;
        factor = show_factor (factor x) ;
        replicate = "1" ;
        bam = mapped_reads_bam x ;
        peaks = Macs2.narrow_peaks (tf_peaks x) ;
      })
  in
  ChIPQC.run samples

let report =
  let open Bistro_utils.Html_report in
  make
    ~title:"Integrated approaches reveal determinants of genome-wide binding and function of the transcription factor Pho4."
    [
      text {|
        This is an attempt at reproducing a paper by Zhou and O'Shea on why
        transcription factors with similar binding sequences are not bound
        to the same genomic sites.
      |} ;
      section "Inferred motifs" ;
      png (Meme_suite.meme_logo (meme `ChIP_Pho4_noPi) 1) ;
    ]
  |> render

let repo = Repo.[
    item [ "report.html" ] report ;
    item [ "macs2" ; "Pho4" ; "noPi" ] (tf_peaks `ChIP_Pho4_noPi) ;
    item [ "meme" ; "Pho4" ; "noPi" ] (meme `ChIP_Pho4_noPi) ;
    item [ "meme_chip" ; "Pho4" ; "noPi" ] (meme_chip `ChIP_Pho4_noPi) ;
    item [ "chIP-QC" ; "Pho4" ; "noPi" ] chipqc ;
  ]

let () =
  Repo.build_main
    ~np ~mem:(`GB 4)
    ~outdir:"res"
    ~loggers:[ Console_logger.create () ]
    repo
