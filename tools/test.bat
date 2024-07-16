cargo build && cargo test && ruby tools\tester.rb %* && cargo run -p dora -- test --package boots dora-boots/boots.dora --test-boots tests/hello-world.dora --gc-verify
