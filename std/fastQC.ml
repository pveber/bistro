open Types
open Bistro
open Bistro.EDSL_sh

let package = Workflow.make ~descr:"fastQC.package" [%sh{|
PREFIX={{ dest }}
TMP={{ tmp }}

set -e

URL=http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v0.10.1.zip
ARCHIVE=`basename ${URL}`
PACKAGE=${ARCHIVE%\.zip}

cd $TMP
wget -O ${ARCHIVE} ${URL}
unzip ${ARCHIVE}

cd FastQC
mkdir -p ${PREFIX}/local/fastqc
cp -r * ${PREFIX}/local/fastqc
chmod 755 ${PREFIX}/local/fastqc/fastqc

mkdir -p $PREFIX/bin
ln -s ${PREFIX}/local/fastqc/fastqc ${PREFIX}/bin/fastqc

rm -rf $TMP
|}]

type report = [`fastQC_report] directory

let run fq = workflow [
    mkdir_p dest ;
    cmd "fastqc" ~path:[package] [
      seq [string "--outdir=" ; dest] ;
      dep fq ;
    ] ;
    rm_rf (dest // "*.zip") ;
    mv (dest // "*_fastqc/*") (dest) ;
    rm_rf (dest // "*_fastqc") ;
  ]


let html_report dir =
  Workflow.extract dir ["fastqc_report.html"]

let per_base_quality dir =
  Workflow.extract dir ["Images" ; "per_base_quality.png"]

let per_base_sequence_content dir =
  Workflow.extract dir ["Images" ; "per_base_sequence_content.png"]

