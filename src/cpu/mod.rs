#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

pub mod aarch64;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Reg(u8);