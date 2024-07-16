cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run -p dora --release -- test --package boots dora-boots/boots.dora --test-boots tests/hello-world.dora
