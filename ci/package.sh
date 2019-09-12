#!/bin/sh -e

src=/home/x/src
bin=/home/x/.cabal/bin
pkg=$PWD/pkg

cd "$src"
cabal v1-install -O2
cp ci/Dockerfile "$pkg/"
mkdir "$pkg/bin"
cp "$bin"/isx-* "$pkg/bin"

ls -AlhR "$pkg"
