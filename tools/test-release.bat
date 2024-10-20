cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run -p dora --release -- test --include-boots --test-boots tests/hello-world.dora
