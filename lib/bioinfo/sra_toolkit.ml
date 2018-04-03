open Core_kernel.Std
open Bistro.EDSL

let env = docker_image ~account:"pveber" ~name:"sra-toolkit" ~tag:"2.8.0" ()

let fastq_dump sra =
  workflow ~descr:"sratoolkit.fastq_dump" [
    cmd ~env "fastq-dump" [ string "-Z" ; dep sra ] ~stdout:dest
  ]

let sra_of_input = function
  | `id id -> string id
  | `file w -> dep w

let fastq_dump_gz input =
  let sra = sra_of_input input in
  workflow ~descr:"sratoolkit.fastq_dump" [
    cmd ~env "fastq-dump" [ string "--gzip" ; string "-Z" ; sra ] ~stdout:dest
  ]

let fastq_dump_pe sra =
  let dir =
    workflow ~descr:"sratoolkit.fastq_dump" [
      mkdir_p dest ;
      cmd ~env "fastq-dump" [
        opt "-O" ident dest ;
        string "--split-files" ;
        dep sra
      ] ;
      mv (dest // "*_1.fastq") (dest // "reads_1.fastq") ;
      mv (dest // "*_2.fastq") (dest // "reads_2.fastq") ;
    ]
  in
  dir / selector ["reads_1.fastq"],
  dir / selector ["reads_2.fastq"]


let fastq_dump_pe_gz input =
  let sra = sra_of_input input in
  let dir =
    workflow ~descr:"sratoolkit.fastq_dump" [
      mkdir_p dest ;
      cmd ~env "fastq-dump" [
        opt "-O" ident dest ;
        string "--split-files" ;
        string "--gzip" ;
        sra ;
      ] ;
      mv (dest // "*_1.fastq*") (dest // "reads_1.fq.gz") ;
      mv (dest // "*_2.fastq*") (dest // "reads_2.fq.gz") ;
    ]
  in
  dir / selector ["reads_1.fq.gz"],
  dir / selector ["reads_2.fq.gz"]

let fastq_dump_to_fasta sra =
  workflow ~descr:"sratoolkit.fastq_dump" [
    cmd ~env "fastq-dump" [
      string "-Z" ;
      string "--fasta" ;
      dep sra
    ] ~stdout:dest
  ]
