RUNS=3

echo "java"
javac fannkuchredux.java
for i in $(seq 1 $RUNS); do time java fannkuchredux 12; done

echo
echo "dora"
pushd ../..
cargo build --release
for i in $(seq 1 $RUNS); do time target/release/dora bench/fannkuchredux/fannkuchredux.dora 12; done
popd

echo
echo "perl"
for i in $(seq 1 $RUNS); do time perl fannkuchredux.pl 12; done
