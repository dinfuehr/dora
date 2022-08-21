# The Core Programming Language

The source repository of the Core Programming Language implementation.
Works on Linux, Windows and macOS (x86\_64 and aarch64).

The source repository of the Core Programming Language [website](https://core-lang.dev) can be found [here](https://github.com/core-lang/core-website).

## Setup
Install Rust nightly through [rustup.rs](http://rustup.rs). Use the specific nightly version listed in the [rust-toolchain](https://github.com/dinfuehr/dora/blob/master/rust-toolchain) file. Core simply uses `cargo` for building:


## Compilation
```
# build in debug and release mode
cargo build && cargo build --release

##  Testing

The test runner is implemented in [Ruby](https://www.ruby-lang.org/) and therefore a Ruby interpreter needs to be installed on your system (e.g. `brew/dnf/apt install ruby`).

# run all tests in debug and release mode (needs Ruby)
tools/test && tools/test-release # Linux and macOS
tools/test.bat && tools/test-release.bat # Windows
```

## Working on the standard library
The standard library (stdlib) is included into the `dora`-binary at compile time.
Changing the stdlib therefore requires recompiling Dora, even though the stdlib is written in Dora.
In order to avoid this recompilation when working on the stdlib, simply pass your working directory of the stdlib to Dora using the `--stdlib` argument.
With this parameter, Dora loads the stdlib from the specified directory instead of the one bundled in the executable.

## Acknowledgement

Core is derived from the Dora programming language, created by [its contributors](https://github.com/dinfuehr/dora/graphs/contributors).
Core will start to diverge substantially in the future, but the language would not exist without the ground work laid in the Dora project.
Thanks to all contributors!

## License

The Core Programming Language is licensed under a mix of MIT and MPL-2.0, as follows:

- Contributions derived from Dora are licensed under [Dora's MIT license](https://github.com/dinfuehr/dora/blob/main/LICENSE.md).

- Contributions made to this repository are licensed under the [MPL-2.0](https://www.mozilla.org/en-US/MPL/2.0/).

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the MPL-2.0 license, shall be licensed under the MPL-2.0 license, without any additional terms or conditions.
