#!/bin/bash
RUNS=3
ARG=20
set -e

echo "java" 1>&2
javac binarytrees.java
for i in $(seq 1 $RUNS); do time java binarytrees $ARG; done

echo
echo "dora" 1>&2
pushd ../..
cargo build --release
for i in $(seq 1 $RUNS); do time target/release/dora --heap-size=512M bench/binarytrees/binarytrees.dora $ARG; done
popd

echo
echo "cacao" 1>&2
javac  -source 1.6 -target 1.6 binarytrees.java
for i in $(seq 1 $RUNS); do time /home/dominik/code/cacao/install/usr/local/cacao/bin/cacao -Xmx=512m binarytrees $ARG; done

echo
echo "go" 1>&2
go build binarytrees.go
for i in $(seq 1 $RUNS); do time ./binarytrees $ARG; done


echo
echo "perl" 1>&2
for i in $(seq 1 $RUNS); do time perl binarytrees.pl $ARG; done
