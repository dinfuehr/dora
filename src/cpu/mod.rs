use std::convert::From;

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

pub mod arm64;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Reg(u8);

impl From<Reg> for u32 {
    fn from(reg: Reg) -> u32 {
        reg.0 as u32
    }
}
