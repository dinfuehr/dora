cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run -p dora --release -- test dora-boots/boots.dora --gc-verify
