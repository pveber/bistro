#!/bin/bash

set -e

jbuilder build @doc

git clone git@github.com:pveber/bistro.git
cd bistro
git checkout gh-pages
rsync -avz ../_build/default/_doc/ ./
git add *
git commit -m "$(cd .. && git log -1 --pretty="%h %s")"
git push origin gh-pages
cd ..
rm -rf bistro
