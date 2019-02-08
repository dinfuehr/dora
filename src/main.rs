extern crate dora;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(dora::run());
}
