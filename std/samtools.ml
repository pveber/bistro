open Core_kernel.Std
open Bistro
open Types
open Bistro.EDSL_sh

let package = Bistro.Workflow.make ~descr:"samtools.package" [%sh{|
PREFIX={{ dest }}

URL=http://sourceforge.net/projects/samtools/files/samtools/0.1.17/samtools-0.1.17.tar.bz2/download
ARCHIVE=`basename ${URL%\/download}`
PACKAGE=${ARCHIVE%\.tar.bz2}

mkdir -p $PREFIX/src
cd $PREFIX/src
wget -O ${ARCHIVE} ${URL}
tar xvfj ${ARCHIVE}
rm $ARCHIVE
cd ${PACKAGE}
make

mkdir -p $PREFIX/bin
cp samtools ${PREFIX}/bin

mkdir -p ${PREFIX}/include/bam
cp *.h ${PREFIX}/include/bam

mkdir -p ${PREFIX}/lib
cp libbam.a ${PREFIX}/lib

make clean
|}]

let samtools subcmd args = cmd "samtools" ~path:[package] (string subcmd :: args)

let sam_of_bam bam =
  workflow ~descr:"samtools.sam_of_bam" [
    samtools "view" [
      opt "-o" ident dest ;
      dep bam ;
    ]
  ]

let indexed_bam_of_sam sam =
  workflow ~descr:"samtools.indexed_bam_of_sam" [
    mkdir_p dest ;
    samtools "view" [
      string "-S -b" ;
      opt "-o" (fun () -> dest // "temp.bam") () ;
      dep sam ;
    ] ;
    samtools "sort" [
      dest // "temp.bam" ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
    rm_rf (dest // "temp.bam") ;
  ]

let sort ?on:order bam =
  workflow ~descr:"samtools.sort" [
    samtools "sort" [
      option (fun o -> flag string "-n" (o = `name)) order ;
      dep bam ;
      dest ;
    ] ;
    mv (seq [dest ; string ".bam"]) dest ;
  ]

let indexed_bam_of_bam bam =
  workflow [
    mkdir_p dest ;
    samtools "sort" [
      dep bam ;
      dest // "reads" ;
    ] ;
    samtools "index" [ dest // "reads.bam" ] ;
  ]

let bam_of_indexed_bam ibam =
  Workflow.extract ibam ["reads.bam"]
