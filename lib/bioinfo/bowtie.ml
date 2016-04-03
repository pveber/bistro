open Core.Std
open Bistro.Std
open Misc.Infix
open Bistro.EDSL_sh

type index = [`bowtie_index] directory

let package = {
  Bistro.pkg_name = "bowtie" ;
  pkg_version = "1.1.2" ;
}

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie_build ?packed ?color fa =
  workflow ~descr:"bowtie_build" ~mem:(3 * 1024) ~pkgs:[package] [
    mkdir_p dest ;
    cmd "bowtie-build" [
      option (flag string "-a -p") packed ;
      option (flag string "--color") color ;
      opt "-f" dep fa ;
      seq [ dest ; string "/index" ]
    ]
  ]

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq.Phred64 -> "--phred64-quals"

let bowtie ?l ?e ?m ?fastq_format ?n ?v ?maxins index fastq_files =
  let args = match fastq_files with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list dep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list dep ~sep:",") fqs2
      ]
  in
  workflow ~descr:"bowtie" ~mem:(3 * 1024) ~pkgs:[package] ~np:8 [
    cmd "bowtie" [
      string "-S" ;
      option (opt "-n" int) n ;
      option (opt "-l" int) l ;
      option (opt "-e" int) e ;
      option (opt "-m" int) m ;
      option (opt "-v" int) v ;
      option (opt "-q" (qual_option % string)) fastq_format ;
      opt "-p" ident np ;
      option (opt "--maxins" int) maxins ;
      seq [dep index ; string "/index"] ;
      args ;
      dest ;
    ]
  ]



















