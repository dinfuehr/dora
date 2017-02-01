#!/bin/bash
RUNS=3
set -e

echo "java" 1>&2
javac binarytrees.java
for i in $(seq 1 $RUNS); do time java binarytrees 20; done

echo
echo "dora" 1>&2
pushd ../..
cargo build --release
for i in $(seq 1 $RUNS); do time target/release/dora bench/binarytrees/binarytrees.dora 20; done
for i in $(seq 1 $RUNS); do time target/release/dora --gc-copy --gc-copy-size=512m bench/binarytrees/binarytrees.dora 20; done
popd

echo
echo "cacao" 1>&2
javac  -source 1.6 -target 1.6 binarytrees.java
for i in $(seq 1 $RUNS); do time /usr/local/cacao/bin/cacao -Xmx=512m binarytrees 20; done

echo
echo "perl" 1>&2
for i in $(seq 1 $RUNS); do time perl binarytrees.pl 20; done
