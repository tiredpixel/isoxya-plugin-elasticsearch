#!/bin/bash -e

src=/home/x/src

cd "$src"
declare -a hsrc
readarray -t hsrc < <(
    find ./*/* -maxdepth 0 -type d \
        ! -path './dist/*' \
        ! -path './lib/snap-extras' \
        -type d -print
)
hlint "${hsrc[@]}"
cabal v1-test --show-details=direct
