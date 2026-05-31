#[cfg(target_arch = "x86_64")]
pub use self::x64::*;
pub use crate::{FReg, Reg};

pub mod x64;

#[cfg(target_arch = "aarch64")]
pub use self::arm64::*;

pub mod arm64;
