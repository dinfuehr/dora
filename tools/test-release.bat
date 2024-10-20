cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run -p dora --release -- test --boots --test-boots tests/hello-world.dora
