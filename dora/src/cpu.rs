use std::convert::From;

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "aarch64")]
pub use self::arm64::*;

#[cfg(target_arch = "aarch64")]
pub mod arm64;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Reg(pub u8);

impl From<Reg> for u32 {
    fn from(reg: Reg) -> u32 {
        reg.0 as u32
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FReg(pub u8);

impl From<FReg> for u32 {
    fn from(reg: FReg) -> u32 {
        reg.0 as u32
    }
}

pub enum Mem {
    // rbp + val1
    Local(i32),

    // reg1 + val1
    Base(Reg, i32),

    // reg1 + reg2 * val1 + val2
    Index(Reg, Reg, i32, i32),

    // reg1 * val1 + val2
    Offset(Reg, i32, i32),
}
