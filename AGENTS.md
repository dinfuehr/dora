# Test
Use "cargo test" to run all unit tests.

Parser, formatter, sema, and bytecode tests are in test/parse, test/fmt, test/sema, and test/bc. Run them with:

```
cargo run -p dora-frontend --bin run-unit-tests [<file-or-dir>...]
```

Without arguments all tests are run. Pass a filename or directory to run specific tests.
For new tests the output file is written automatically. Use --force to regenerate output files.

Full runtime tests are in test/rt. Tests can be run with tools/rt.

tools/test runs all these tests after each other. This is also the script CI bots use to make sure everything works.

# Format
Run "cargo fmt" on your Rust changes.
