RUNS=3

echo "java"
javac binarytrees.java
for i in $(seq 1 $RUNS); do time java binarytrees 20; done

echo
echo "dora"
pushd ../..
cargo build --release
for i in $(seq 1 $RUNS); do time target/release/dora bench/binarytrees/binarytrees.dora 20; done
popd

echo
echo "perl"
for i in $(seq 1 $RUNS); do time perl binarytrees.pl 20; done
