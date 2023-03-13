pub mod language;
pub mod utils;

pub const STDLIB: &[(&str, &str)] = &include!(concat!(env!("OUT_DIR"), "/dora_stdlib_bundle.rs"));

pub use utils::{GrowableVec, Id, MutableVec};
