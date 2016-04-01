open Types
open Bistro
open Bistro.EDSL_sh

let package = {
  pkg_name = "fastqc" ;
  pkg_version = "0.10.1" ;
}

type report = [`fastQC_report] directory

let run fq = Workflow.make ~pkgs:[package] ~descr:"fastQC" [%bash{|
DEST={{ dest }}
mkdir $DEST

set -e
fastqc --outdir=$DEST {{ dep fq }}
rm -rf $DEST/*.zip
mv $DEST/*_fastqc/* $DEST
rm -rf $DEST/*_fastqc
|}]

let html_report =
  selector ["fastqc_report.html"]

let per_base_quality =
  selector ["Images" ; "per_base_quality.png"]

let per_base_sequence_content =
  selector ["Images" ; "per_base_sequence_content.png"]

