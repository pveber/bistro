open Core_kernel.Std
open Types
open Bistro.EDSL_sh
open Misc.Infix

let pysam_package = Bistro.Workflow.make [%sh{|
PREFIX={{ dest }}

URL=https://pypi.python.org/packages/source/p/pysam/pysam-0.8.1.tar.gz#md5=9b2c7b4c1ea63841815725557da188fb
ARCHIVE=pysam-0.8.1.tar.gz
PACKAGE=pysam

mkdir -p $PREFIX/src
cd $PREFIX/src
wget -O ${ARCHIVE} ${URL}
tar xvfz ${ARCHIVE}
rm $ARCHIVE
cd ${ARCHIVE%\.tar.gz}
PYTHONVERSION=`python --version 2>&1 |grep -o '[0-9]\.[0-9]'`
PYTHONLIBDIR=$PREFIX/lib/python${PYTHONVERSION}/site-packages
PYTHONPATH=$PYTHONLIBDIR:$PYTHONPATH
mkdir -p $PYTHONLIBDIR
python setup.py install --prefix ${PREFIX}
|}]

let package = Bistro.Workflow.make [%sh{|
PREFIX={{ dest }}

URL=https://pypi.python.org/packages/source/H/HTSeq/HTSeq-0.6.1p1.tar.gz#md5=c44d7b256281a8a53b6fe5beaeddd31c
ARCHIVE=HTSeq-0.6.1p1.tar.gz
PACKAGE=htseq

mkdir -p $PREFIX/src
cd $PREFIX/src
wget -O ${ARCHIVE} ${URL}
tar xvfz ${ARCHIVE}
rm $ARCHIVE
cd ${ARCHIVE%\.tar.gz}
PYTHONVERSION=`python --version 2>&1 |grep -o '[0-9]\.[0-9]'`
PYTHONLIBDIR=$PREFIX/lib/python${PYTHONVERSION}/site-packages
PYTHONPATH=$PYTHONLIBDIR:$PYTHONPATH
mkdir -p $PYTHONLIBDIR
python setup.py install --prefix ${PREFIX}
|}]

class type count_tsv = object
  inherit [ < header : [`no] ; .. > ] tsv
  method f1 : string
  method f2 : int
end

let string_of_mode = function
  | `union -> "union"
  | `intersection_strict -> "intersection-strict"
  | `intersection_nonempty -> "intersection-nonempty"

let string_of_strandedness = function
  | `yes -> "yes"
  | `no -> "no"
  | `reverse -> "reverse"

let string_of_order = function
  | `name -> "name"
  | `position -> "pos"

let count ?order ?mode ?stranded ?feature_type ?minaqual ?idattribute alns gff =
  let format, input = match alns with
    | `sam sam -> "sam", dep sam
    | `bam bam -> "bam", dep bam
  in
  workflow [
    cmd ~pythonpath:[package ; pysam_package] ~path:[package] ~stdout:dest "htseq-count" [
      opt "-f" string format ;
      option (opt "-m" (string_of_mode % string)) mode ;
      option (opt "-r" (string_of_order % string)) order ;
      option (opt "-s" (string_of_strandedness % string)) stranded ;
      option (opt "-t" string) feature_type ;
      option (opt "-a" int) minaqual ;
      option (opt "-i" string) idattribute ;
      input ;
      dep gff ;
    ]
  ]
