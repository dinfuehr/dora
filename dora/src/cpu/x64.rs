use std::sync::atomic::{compiler_fence, Ordering};

pub use self::param::*;
pub use self::reg::*;
use lazy_static::lazy_static;
use raw_cpuid::{CpuId, ExtendedFeatures, FeatureInfo};

pub mod asm;
pub mod param;
pub mod reg;

pub fn flush_icache(_: *const u8, _: usize) {
    // no flushing needed on x86_64, but emit compiler barrier
    compiler_fence(Ordering::SeqCst);
}

pub fn has_round() -> bool {
    *HAS_ROUND
}

pub fn has_popcnt() -> bool {
    *HAS_POPCNT
}

pub fn has_lzcnt() -> bool {
    // Too conservative, but we currently lack an easy way to check support for ABM instructions:
    // Revisit after https://github.com/gz/rust-cpuid/issues/30 is resolved.
    *HAS_TZCNT
}

pub fn has_tzcnt() -> bool {
    *HAS_TZCNT
}

lazy_static! {
static ref FEATURES: FeatureInfo = CpuId::new().get_feature_info().unwrap();
static ref FEATURES_EXTENDED: ExtendedFeatures = CpuId::new().get_extended_feature_info().unwrap();

// support for floating point rounding
static ref HAS_ROUND: bool = FEATURES.has_sse41();
static ref HAS_POPCNT: bool = FEATURES.has_popcnt();
static ref HAS_TZCNT: bool = FEATURES_EXTENDED.has_bmi1();
// support for MULX, RORX, SARX, SHRX, SHLX
static ref HAS_X_OPS: bool = FEATURES_EXTENDED.has_bmi2();
}
