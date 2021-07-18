#!/bin/bash

git submodule init
git submodule update

# publish local branch of evilplot
pushd lib/evilplot
sbt publishLocal
popd


