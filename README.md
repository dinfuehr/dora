# Dora [![Build Status](https://travis-ci.org/dinfuehr/dora-rust.svg?branch=master)](https://travis-ci.org/dinfuehr/dora)

JIT-compiler for the programming language Dora implemented in Rust. Build with:

## Dependencies
You need to install dependencies (which is just [capstone](https://github.com/aquynh/capstone) for now):

```
$ git clone https://github.com/aquynh/capstone
$ cd capstone && git checkout 3.0.4 && sudo make install
```

## Compilation & Testing
Install current Rust Nightly via [rustup.rs](http://rustup.rs). The nightly version of
Rust is needed because Dora uses some unstable features of Rust (e.g. inline assembly).

Dora uses [cargo](http://crates.io) for building, which is bundled with Rust:

```
# right now specific nightly version is needed
rustup update nightly-2016-10-21
rustup override set nightly-2016-10-21

cargo build
cargo test # runs tests

./test # builds + runs unit tests + runs test suite
./test-release # builds release mode + runs unit tests + runs test suite
```
