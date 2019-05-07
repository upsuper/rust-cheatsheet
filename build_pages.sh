#!/usr/bin/env bash

set -xe

mkdir out
cp static/* out
for f in data/*.yml; do
  cargo run -- "${f}" "out/$(basename "${f}" .yml).html"
done
