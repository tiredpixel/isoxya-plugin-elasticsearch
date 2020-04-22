#!/bin/sh -e

src=/home/x/src
bin=/home/x/.cabal/bin
pkg=$PWD/pkg
meta=$PWD/meta

cd "$src"
cabal v1-install -O2
cp ci/Dockerfile "$pkg/"
mkdir "$pkg/bin"
cp "$bin"/isx-* "$pkg/bin"
cp "$meta/version" "$pkg/.version"

ls -AlhR "$pkg"
