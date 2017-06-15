#!/bin/bash
RUNS=3
ARG=12

echo "java" 1>&2
javac fannkuchredux.java
for i in $(seq 1 $RUNS); do time java fannkuchredux $ARG; done

echo
echo "dora" 1>&2
pushd ../..
cargo build --release
for i in $(seq 1 $RUNS); do time target/release/dora bench/fannkuchredux/fannkuchredux.dora $ARG; done
popd

echo "cacao" 1>&2
javac -source 1.6 -target 1.6 fannkuchredux.java
for i in $(seq 1 $RUNS); do time /usr/local/cacao/bin/cacao fannkuchredux $ARG; done

echo
echo "perl" 1>&2
for i in $(seq 1 $RUNS); do time perl fannkuchredux.pl $ARG; done
