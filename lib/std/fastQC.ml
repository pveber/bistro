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
cd $PREFIX/bin
ln -s ../local/fastqc/fastqc .

rm -rf $TMP
|}]

type report = [`fastQC_report] directory

let run fq = Workflow.make ~descr:"fastQC" [%bash{|
DEST={{ dest }}
mkdir $DEST

set -e
export PATH={{ dep package }}/bin:$PATH
fastqc --outdir=$DEST {{ dep fq }}
rm -rf $DEST/*.zip
mv $DEST/*_fastqc/* $DEST
rm -rf $DEST/*_fastqc
|}]

(* [ *)
(*     mkdir_p dest ; *)
(*     cmd "fastqc" ~path:[package] [ *)
(*       seq [string "--outdir=" ; dest] ; *)
(*       dep fq ; *)
(*     ] ; *)
(*     rm_rf (dest // "*.zip") ; *)
(*     mv (dest // "*_fastqc/*") (dest) ; *)
(*     rm_rf (dest // "*_fastqc") ; *)
(*   ] *)


let html_report =
  selector ["fastqc_report.html"]

let per_base_quality =
  selector ["Images" ; "per_base_quality.png"]

let per_base_sequence_content =
  selector ["Images" ; "per_base_sequence_content.png"]

