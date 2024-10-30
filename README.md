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
