open Bistro
open Bistro_bioinfo

let sample = Sra.fetch_srr "SRR217304"                         (* Fetch a sample from the SRA database *)
let sample_fq = Sra_toolkit.fastq_dump sample                  (* Convert it to FASTQ format *)
let genome = Ucsc_gb.genome_sequence `sacCer2                  (* Fetch a reference genome *)
let bowtie2_index = Bowtie2.bowtie2_build genome               (* Build a Bowtie2 index from it *)
let sample_sam =                                               (* Map the reads on the reference genome *)
  Bowtie2.bowtie2 bowtie2_index (`single_end [ sample_fq ])
let sample_bam =                                               (* Convert SAM file to BAM format *)
  Samtools.(indexed_bam_of_sam sample_sam |> indexed_bam_to_bam)
let sample_peaks = Macs2.(callpeak bam [ sample_bam ])             (* Call peaks on mapped reads *)

let repo = Repo.[
  [ "peaks" ] %> sample_peaks
]

(** Actually run the pipeline *)
let () = Repo.build ~outdir:"res" repo
