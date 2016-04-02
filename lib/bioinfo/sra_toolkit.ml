open Core_kernel.Std
open Bistro
open Bistro.EDSL_sh

let package = {
  pkg_name = "sra-tools" ;
  pkg_version = "2.5.7" ;
}


let fastq_dump sra =
  workflow ~pkgs:[package] ~descr:"sratoolkit.fastq_dump" [
    cmd "fastq-dump" [ string "-Z" ; dep sra ] ~stdout:dest
  ]

let fastq_dump_pe sra =
  let dir =
    workflow ~pkgs:[package] ~descr:"sratoolkit.fastq_dump" [
      mkdir_p dest ;
      cmd "fastq-dump" [
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
