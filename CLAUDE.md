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

## AOT tests

AOT compilation must be explicitly enabled per test file by adding `//= aot` as the first line. Tests without this annotation are skipped in `tools/rt --aot` runs. AOT tests run on x64 and arm64 Linux.

To compile a file in AOT mode and run the resulting binary:

```
target/debug/dora compile test/rt/trait/trait-object1.dora -o /tmp/trait-object1
/tmp/trait-object1
```

To run a specific test in AOT mode through the test runner:

```
tools/rt --aot test/rt/trait/trait-object1.dora
```

# Bytecode definitions (bytecode.toml)
`tools/bytecode.toml` is the source of truth for enum/constant values shared between Rust and Dora.
Run `./tools/bytecode-gen.py` to regenerate two files from it:
- `pkgs/boots/bytecode/opcode.dora` (Dora constants + name functions)
- `dora-bytecode/src/opcode.rs` (Rust constants)

Do not edit those files by hand. Edit `bytecode.toml` and re-run the generator.

The `[Intrinsic]` variant order in `bytecode.toml` must match the enum variant order in `dora-runtime/src/vm/known.rs` — the numeric indices are derived from position.

All sections in `bytecode.toml` (including `[RelocationKind]`, `[RuntimeFunction]`, etc.) generate constants in the opcode files. To add a new constant, add the variant to the appropriate section in `bytecode.toml` and re-run the generator.

Use `./tools/bytecode-gen.py --check` to verify generated files are up to date.

# Format
Run "cargo fmt" on your Rust changes.
