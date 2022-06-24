# Dora

[![Join the chat at https://gitter.im/dora-lang/dora](https://badges.gitter.im/dora-lang/Lobby.svg)](https://gitter.im/dora-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Actions Status](https://github.com/dinfuehr/dora/workflows/ci/badge.svg)](https://github.com/dinfuehr/dora/actions)

JIT-compiler for the programming language Dora implemented in Rust. Works on
Linux, Windows and macOS (x86\_64 and aarch64). Build with:

## Compilation & Testing

Install Rust stable with the help of [rustup.rs](http://rustup.rs). Dora uses
`cargo` for building:

```
# build in debug and release mode
cargo build && cargo build --release

# run all tests in debug and release mode (needs Ruby)
tools/test && tools/test-release # Linux and macOS
tools/test.bat && tools/test-release.bat # Windows
```

Note that the test runner is implemented in [Ruby](https://www.ruby-lang.org/)
and therefore a Ruby interpreter needs to be installed on your system (e.g.
`brew/dnf/apt install ruby`).

## Working on the standard library

The standard library (stdlib) is included into the `dora`-binary at compile
time. Changing the stdlib therefore requires recompiling Dora, even though the
stdlib is written in Dora. In order to avoid this recompilation when working on
the stdlib, simply pass your working directory of the stdlib to Dora using the
`--stdlib` argument. With this parameter, Dora loads the stdlib from the
specified directory instead of the one bundled in the executable.
