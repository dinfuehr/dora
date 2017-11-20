# Dora [![Build Status](https://travis-ci.org/dinfuehr/dora.svg?branch=master)](https://travis-ci.org/dinfuehr/dora)

JIT-compiler for the programming language Dora implemented in Rust.
Works on Linux (x86\_64, aarch64) and macOS (x86\_64).
Build with:

## Dependencies
You need to install these dependencies:

[llvm](http://llvm.org) - Download LLVM 5.0.0 from [llvm.org](http://releases.llvm.org/5.0.0/llvm-5.0.0.src.tar.xz).
Unpack the archive and switch into the source directory `$LLVM_SRC_DIR` (e.g. `$HOME/llvm-5.0.0.src`).
We will now build & install LLVM to `$LLVM_INSTALL_DIR` (e.g. `$HOME/llvm-5.0.0`).

```
cd $LLVM_SRC_DIR # change into directory with LLVM source
cmake -G Ninja . -DLLVM_ENABLE_ASSERTIONS=on \
                 -DCMAKE_INSTALL_PREFIX=$LLVM_INSTALL_DIR \
                 -DCMAKE_BUILD_TYPE=Debug \
                 -DLLVM_TARGETS_TO_BUILD=X86 \
cmake --build . --target install
```

You can find more information on building LLVM, in its [documentation](http://llvm.org/docs/CMake.html).

[capstone](https://github.com/aquynh/capstone)

```
$ git clone https://github.com/aquynh/capstone
$ cd capstone && git checkout 3.0.4 && sudo make install

# on MacOS capstone can be installed via homebrew
$ brew install capstone
```

[ruby](https://www.ruby-lang.org/) - used to run tests


## Compilation & Testing
Install current Rust Nightly via [rustup.rs](http://rustup.rs). The nightly version of
Rust is needed because Dora uses some unstable features of Rust (e.g. inline assembly).

Dora uses [cargo](http://crates.io) for building, which is bundled with Rust:

```
# install last nightly and use it for this project
rustup update nightly
rustup override set nightly

# run all tests in debug and release mode
LLVM_SYS_50_PREFIX=$LLVM_INSTALL_DIR ./test
LLVM_SYS_50_PREFIX=$LLVM_INSTALL_DIR ./test-release
```
