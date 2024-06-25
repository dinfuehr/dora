cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run -p dora --release -- test-boots --package boots dora-boots/boots.dora --gc-verify
