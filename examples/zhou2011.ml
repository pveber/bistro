(** 
   Paper: https://www.ncbi.nlm.nih.gov/pubmed/21700227
   Datasets: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506
*)
open Core
open Bistro
open Bistro_bioinfo
open Bistro_utils

let np = 4
let genome = Ucsc_gb.genome_sequence `sacCer2
let genome_2bit = Ucsc_gb.genome_2bit_sequence `sacCer2

let chIP_design tf condition = match tf, condition with
  | `Pho4, `noPi -> (`ChIP_Pho4_noPi, `Input_Pho4_NoPi)

let srr_id = function
  | `ChIP_Pho4_noPi -> [ "SRR217304" ; "SRR217305" ]
  | `Input_Pho4_NoPi -> [ "SRR217319" ]

let sra x = List.map (srr_id x) ~f:(fun id ->
    Sra.fetch_srr (Workflow.string id)
  )

let fastq x = List.map ~f:Sra_toolkit.fastq_dump (sra x)

let bowtie_index = Bowtie.bowtie_build genome

let mapped_reads x =
  Bowtie.bowtie ~v:2 bowtie_index (`single_end (fastq x))

let tf_peaks tf condition =
  let treatment_sample, control_sample = chIP_design tf condition in
  let treatment = mapped_reads treatment_sample in
  let control = mapped_reads control_sample in
  Macs2.callpeak ~mfold:(1,100) Macs2.sam ~control:[ control ] [ treatment ]

let peak_sequences ~radius tf condition =
  let summits = Macs2.peak_summits (tf_peaks tf condition) in
  let regions = Bedtools.(slop ~mode:(`both radius) bed summits (Ucsc_gb.fetchChromSizes `sacCer2)) in
  Ucsc_gb.twoBitToFa genome_2bit (Bed.keep4 regions)

let meme tf condition =
  peak_sequences ~radius:50 tf condition
  |> Meme_suite.meme ~nmotifs:3 ~minw:5 ~maxw:15 ~revcomp:true ~alphabet:`dna ~maxsize:1_000_000

let meme_chip tf condition =
  peak_sequences ~radius:50 tf condition
  |> Meme_suite.meme_chip
    ~meme_nmotifs:3 ~meme_minw:5 ~meme_maxw:15

let repo = Repo.[
    item [ "peaks" ; "Pho4" ; "noPi" ] (tf_peaks `Pho4 `noPi) ;
    item [ "motifs" ; "meme" ; "Pho4" ; "noPi" ] (meme `Pho4 `noPi) ;
    item [ "motifs" ; "meme_chip" ; "Pho4" ; "noPi" ] (meme_chip `Pho4 `noPi) ;
  ]

let () =
  Repo.build_main
    ~np:4 ~mem:(`GB 4)
    ~outdir:"res"
    ~loggers:[ Console_logger.create () ] 
    repo    
