open Types
open Bistro.EDSL_sh

let package_script = Unix_utils.wget "https://raw.githubusercontent.com/pveber/compbio-scripts/master/bedtools-install/2.11.2/bedtools-install.sh"

let package =
  Bistro.Workflow.make ~descr:"bedtools.package" [%sh{|
PREFIX={{ dest }}

set -e

URL=http://bedtools.googlecode.com/files/BEDTools.v2.11.2.tar.gz
ARCHIVE=`basename ${URL}`
PACKAGE=${ARCHIVE%\.tar.gz}

mkdir -p $PREFIX/src
cd $PREFIX/src
wget -O ${ARCHIVE} ${URL}
tar xvfz ${ARCHIVE}
rm $ARCHIVE
cd BEDTools-Version-2.11.2
make

mkdir -p ${PREFIX}/bin
cp bin/* ${PREFIX}/bin
make clean
|}]
