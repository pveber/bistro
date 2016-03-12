open Bistro.Std
open Misc.Infix
open Bistro.EDSL_sh

type index = [`bowtie_index] directory

let package = Bistro.Workflow.make ~descr:"bowtie.package" [%sh{|
PREFIX={{ dest }}

set -e

mkdir -p $PREFIX
cd $PREFIX
wget -O bowtie.zip "http://downloads.sourceforge.net/project/bowtie-bio/bowtie/1.0.1/bowtie-1.0.1-linux-x86_64.zip?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fbowtie-bio%2Ffiles%2Fbowtie%2F1.0.1%2F&ts=1396093686&use_mirror=freefr" || die "failed to download archive"
unzip bowtie.zip
rm bowtie.zip
mv bowtie-1.0.1 bin
|}]

(* memory bound correspond to storing a human index in memory, following bowtie manual *)
let bowtie_build ?packed ?color fa =
  workflow ~descr:"bowtie_build" ~mem:(3 * 1024) [
    mkdir_p dest ;
    cmd "bowtie-build" ~path:[package] [
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

let bowtie ?l ?e ?m ?fastq_format ?n ?v ?p ?maxins index fastq_files =
  let args = match fastq_files with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list dep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list dep ~sep:",") fqs2
      ]
  in
  workflow ~descr:"bowtie" ~mem:(3 * 1024) ?np:p [
    cmd "bowtie" ~path:[package] [
      string "-S" ;
      option (opt "-n" int) n ;
      option (opt "-l" int) l ;
      option (opt "-e" int) e ;
      option (opt "-m" int) m ;
      option (opt "-v" int) v ;
      option (opt "-q" (qual_option % string)) fastq_format ;
      option (opt "-p" int) p ;
      option (opt "--maxins" int) maxins ;
      seq [dep index ; string "/index"] ;
      args ;
      dest ;
    ]
  ]



















