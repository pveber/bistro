open Core_kernel.Std
open Types
open Bistro
open Bistro.EDSL_sh

let package = Workflow.make
    ~descr:"tophat.package"
    [%sh{|\
PREFIX={{ dest }}
TMP={{ tmp }}

URL=http://ccb.jhu.edu/software/tophat/downloads/tophat-2.0.13.Linux_x86_64.tar.gz
ARCHIVE=`basename ${URL}`
PACKAGE=${ARCHIVE%\.tar.gz}

cd $TMP
wget ${URL} || die "failed to fetch ${PACKAGE}"
tar xvfz ${ARCHIVE}
cd ${PACKAGE}
rm README AUTHORS COPYING

mkdir -p $PREFIX/bin
cp * ${PREFIX}/bin
|}]


let tophat1 ?num_threads ?color index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow ?np:num_threads ~mem:(4 * 1024) ~timeout:24 [
    cmd ~path:[package ; Bowtie.package ; Samtools.package] "tophat" [
      option (opt "--num-threads" int) num_threads ;
      option (flag string "--color") color ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let tophat2 ?num_threads index fqs =
  let args = match fqs with
    | `single_end fqs -> list dep ~sep:"," fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        list dep ~sep:"," fqs1 ;
        string " " ;
        list dep ~sep:"," fqs2
      ]
  in
  workflow ?np:num_threads ~mem:(4 * 1024) ~timeout:24 [
    cmd ~path:[package ; Bowtie2.package ; Samtools.package] "tophat2" [
      option (opt "--num-threads" int) num_threads ;
      opt "--output-dir" ident dest ;
      seq [ dep index ; string "/index" ] ;
      args
    ]
  ]

let accepted_hits = selector ["accepted_hits.bam"]

let junctions = selector ["junctions.bed"]

