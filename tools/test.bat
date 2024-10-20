cargo build && cargo test && ruby tools\tester.rb %* && cargo run -p dora -- test --include-boots --test-boots tests/hello-world.dora --gc-verify
