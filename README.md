# Dora [![Build Status](https://travis-ci.org/dinfuehr/dora-rust.svg?branch=master)](https://travis-ci.org/dinfuehr/dora-rust)

JIT-compiler for the programming language Dora implemented in Rust. Build with:

## Dependencies
You need to install dependencies (which is just [capstone](https://github.com/aquynh/capstone) for now):

```
$ git clone https://github.com/aquynh/capstone
$ cd capstone && git checkout 3.0.4 && sudo make install
```

## Compilation & Testing
Dora uses [cargo](http://crates.io) for building:

```
cargo build
cargo test # runs tests
```
