#!/bin/bash

## R-install -- installation script for R and some libraries
## 
## Usage:
##   R-install PREFIX
##
## if it exists, PREFIX should be a directory


set -e

if [ ! -n "$1" ]
then
    grep "^##" $0
    exit 1
fi

PREFIX=$1
if [ -a $PREFIX ] && [ ! -d $PREFIX ]
then
    echo "Destination $PREFIX exists and isn't a directory."
    exit 1
fi

# make PREFIX path absolute
PREFIX=`readlink -f $PREFIX`

mkdir -p $PREFIX
cd $PREFIX
mkdir -p usr/src
cd usr/src
wget http://cran.univ-lyon1.fr/src/base/R-3/R-3.2.2.tar.gz
tar xvfz R-3.2.2.tar.gz
rm -f R-3.2.2.tar.gz
cd R-3.2.2
./configure --prefix=$PREFIX --with-x=no --enable-R-shlib --with-cairo --with-libpng
cd src/nmath/standalone
make
make install
cd -
make
make install
echo install-pc
cd src/unix
make install-pc
cd -
#echo "install.packages(c('gridExtra', 'dplyr', 'coda', 'ggplot2', 'rjags', 'fitdistrplus', 'actuar', 'stringr', 'knitr', 'roxygen2'),repos='http://cran.univ-lyon1.fr')" | R --vanilla
echo "source('https://bioconductor.org/biocLite.R') ; biocLite(c('DESeq2'))" | R --vanilla
