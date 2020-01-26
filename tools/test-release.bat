cargo build --release && cargo test --release && ruby tools\tester.rb --release %* && cargo run --release -- test dora-boots
