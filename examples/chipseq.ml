(** 
   Paper: https://www.ncbi.nlm.nih.gov/pubmed/21700227
   Datasets: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE29506
*)
open Bistro_bioinfo
open Bistro_utils

let treatment_id = "SRR217304"
let control_id = "SRR217324"

let genome = Ucsc_gb.genome_sequence `sacCer2
let bowtie_index = Bowtie.bowtie_build genome

let mapped_reads srrid =
  let sra = Sra.fetch_srr srrid in
  let fastq = Sra_toolkit.fastq_dump sra in
  Bowtie.bowtie ~v:1 bowtie_index (`single_end [ fastq ])

let macs2 =
  Macs2.callpeak ~qvalue:1e-10 ~mfold:(1,100) Macs2.sam
    ~control:[ mapped_reads control_id ]
    [ mapped_reads treatment_id ]

let peak_summits =
  Bed.keep4 (Macs2.peak_summits macs2)

let chrom_sizes = Ucsc_gb.fetchChromSizes `sacCer2

let peak_regions =
  Bedtools.(slop ~mode:(`both 50) bed peak_summits chrom_sizes)

let genome_2bit = Ucsc_gb.genome_2bit_sequence `sacCer2
let peak_sequences = Ucsc_gb.twoBitToFa genome_2bit peak_regions
let meme = Meme_suite.meme ~nmotifs:1 ~minw:7 ~maxw:7 ~revcomp:true ~alphabet:`dna peak_sequences

let repo = Repo.[
    item [ "peaks" ] macs2 ;
    item [ "sequences" ] peak_sequences ;
    item [ "motif" ] meme ;
  ]

let () =
  Repo.build_main
    ~collect:true
    ~allowed_containers:[`Singularity]
    ~np:4 ~mem:(`GB 4)
    ~outdir:"res"
    ~loggers:[ Console_logger.create () ] 
    repo
