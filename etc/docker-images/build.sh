#!/bin/bash

PKG=`dirname $1`
TAG=`basename $1`

cd $PKG/$TAG
docker build -t pveber/$PKG:$TAG .
