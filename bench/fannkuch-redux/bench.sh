echo "java"
javac fannkuchredux.java
for i in {1..5}; do time java fannkuchredux 12; done

echo "cacao"
javac -source 1.6 -target 1.6 fannkuchredux.java
for i in {1..5}; do time cacao fannkuchredux 12; done

echo "dora"
cargo build --release
for i in {1..5}; do time cargo run fannkuchredux.dora 12; done

echo "perl"
for i in {1..5}; do time perl fannkuchredux.pl 12; done
