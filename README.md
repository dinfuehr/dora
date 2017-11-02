# Dora [![Build Status](https://travis-ci.org/dinfuehr/dora.svg?branch=master)](https://travis-ci.org/dinfuehr/dora)

JIT-compiler for the programming language Dora implemented in Rust.
Works on Linux (x86\_64, aarch64) and macOS (x86\_64).
Build with:

## Dependencies
You need to install dependencies:

[capstone](https://github.com/aquynh/capstone)

```
$ git clone https://github.com/aquynh/capstone
$ cd capstone && git checkout 3.0.4 && sudo make install

# on MacOS capstone can be installed via homebrew
$ brew install capstone
```
[ruby](https://www.ruby-lang.org/en/documentation/installation) - used to run tests


## Compilation & Testing
Install current Rust Nightly via [rustup.rs](http://rustup.rs). The nightly version of
Rust is needed because Dora uses some unstable features of Rust (e.g. inline assembly).

Dora uses [cargo](http://crates.io) for building, which is bundled with Rust:

```
# install last nightly and use it for this project
rustup update nightly
rustup override set nightly

# run all tests in debug and release mode
./test && ./test-release
```
