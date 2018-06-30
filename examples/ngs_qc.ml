open Bistro
open Bistro_bioinfo

let sample = Sra.fetch_srr "SRR217304"
let sample_fq = Sra_toolkit.fastq_dump sample
let qc = FastQC.run sample_fq

let repo = Repo.[
    ["qc"] %> qc ;
  ]

let () = Repo.build ~outdir:"res" repo
