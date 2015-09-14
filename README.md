# Dora [![Build Status](https://travis-ci.org/dinfuehr/dora-rust.svg?branch=master)](https://travis-ci.org/dinfuehr/dora-rust)

JIT-compiler for the programming language Dora implemented in Rust. Build with:

```
cargo build
cargo test # runs tests
```

You also need to install dependencies (this this just [capstone](https://github.com/aquynh/capstone) for now):

```
$ git clone https://github.com/aquynh/capstone
$ cd capstone && git checkout 3.0.4 && sudo make install
```
