use lazy_static::lazy_static;
use raw_cpuid::{CpuId, ExtendedFeatures, FeatureInfo};
use std::sync::atomic::{compiler_fence, Ordering};

use crate::asm::Register;
use crate::ty::SourceType;

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
}

// first param offset to rbp is +16,
// rbp+0 -> saved rbp
// rbp+8 -> return address
pub static PARAM_OFFSET: i32 = 16;

// on x64 each parameter needs exactly 8 bytes
pub fn next_param_offset(param_offset: i32, _: SourceType) -> i32 {
    param_offset + 8
}

use crate::cpu::{FReg, Reg};

#[cfg(target_family = "unix")]
pub static REG_PARAMS: [Reg; 6] = [RDI, RSI, RDX, RCX, R8, R9];
#[cfg(target_family = "windows")]
pub static REG_PARAMS: [Reg; 4] = [RCX, RDX, R8, R9];

#[cfg(target_family = "unix")]
pub static FREG_PARAMS: [FReg; 8] = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7];
#[cfg(target_family = "windows")]
pub static FREG_PARAMS: [FReg; 4] = [XMM0, XMM1, XMM2, XMM3];

#[cfg(target_family = "unix")]
pub static CCALL_REG_PARAMS: [Reg; 6] = [RDI, RSI, RDX, RCX, R8, R9];
#[cfg(target_family = "windows")]
pub static CCALL_REG_PARAMS: [Reg; 4] = [RCX, RDX, R8, R9];

#[cfg(target_family = "unix")]
pub static CCALL_FREG_PARAMS: [FReg; 8] = [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7];
#[cfg(target_family = "windows")]
pub static CCALL_FREG_PARAMS: [FReg; 4] = [XMM0, XMM1, XMM2, XMM3];

pub const REG_RESULT: Reg = RAX;
pub const REG_TMP1: Reg = R10;
pub const REG_TMP2: Reg = R11;
pub const REG_SP: Reg = RSP;
pub const REG_FP: Reg = RBP;
pub const REG_THREAD: Reg = R15;

#[cfg(target_family = "unix")]
pub static SCRATCH: [Reg; 4] = [RDI, RSI, RDX, RCX];
#[cfg(target_family = "windows")]
pub static SCRATCH: [Reg; 4] = [RCX, RDX, R8, R9];

pub const FREG_RESULT: FReg = XMM0;

#[cfg(target_family = "unix")]
pub const FREG_TMP1: FReg = XMM8; // shall not overlap with argument registers
#[cfg(target_family = "windows")]
pub const FREG_TMP1: FReg = XMM4; // shall not overlap with argument registers

pub const STACK_FRAME_ALIGNMENT: usize = 16;

pub const RAX: Reg = Reg(0);
pub const RCX: Reg = Reg(1);
pub const RDX: Reg = Reg(2);
pub const RBX: Reg = Reg(3);
pub const RSP: Reg = Reg(4);
pub const RBP: Reg = Reg(5);
pub const RSI: Reg = Reg(6);
pub const RDI: Reg = Reg(7);

pub const R8: Reg = Reg(8);
pub const R9: Reg = Reg(9);
pub const R10: Reg = Reg(10);
pub const R11: Reg = Reg(11);
pub const R12: Reg = Reg(12);
pub const R13: Reg = Reg(13);
pub const R14: Reg = Reg(14);
pub const R15: Reg = Reg(15);

pub const RIP: Reg = Reg(16);

pub const XMM0: FReg = FReg(0);
pub const XMM1: FReg = FReg(1);
pub const XMM2: FReg = FReg(2);
pub const XMM3: FReg = FReg(3);
pub const XMM4: FReg = FReg(4);
pub const XMM5: FReg = FReg(5);
pub const XMM6: FReg = FReg(6);
pub const XMM7: FReg = FReg(7);
pub const XMM8: FReg = FReg(8);
pub const XMM9: FReg = FReg(9);
pub const XMM10: FReg = FReg(10);
pub const XMM11: FReg = FReg(11);
pub const XMM12: FReg = FReg(12);
pub const XMM13: FReg = FReg(13);
pub const XMM14: FReg = FReg(14);
pub const XMM15: FReg = FReg(15);

impl Reg {
    // these four register need sometimes special treatment: e.g. because of bl vs bh
    // for byte operations
    pub fn is_basic_reg(self) -> bool {
        self == RAX || self == RBX || self == RCX || self == RDX
    }

    pub fn int(self) -> u8 {
        assert!(self != RIP);

        self.0
    }

    pub fn msb(self) -> u8 {
        assert!(self != RIP);

        (self.int() >> 3) & 0x01
    }

    pub fn and7(self) -> u8 {
        assert!(self != RIP);

        self.int() & 0x07
    }
}

impl From<Reg> for Register {
    fn from(reg: Reg) -> Register {
        Register::new(reg.0)
    }
}

impl FReg {
    pub fn msb(self) -> u8 {
        (self.0 >> 3) & 0x01
    }

    pub fn and7(self) -> u8 {
        self.0 & 0x07
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int() {
        assert_eq!(0, RAX.int());
        assert_eq!(1, RCX.int());
        assert_eq!(2, RDX.int());
        assert_eq!(3, RBX.int());
        assert_eq!(4, RSP.int());
        assert_eq!(5, RBP.int());
        assert_eq!(6, RSI.int());
        assert_eq!(7, RDI.int());
        assert_eq!(8, R8.int());
        assert_eq!(9, R9.int());
        assert_eq!(10, R10.int());
        assert_eq!(11, R11.int());
        assert_eq!(12, R12.int());
        assert_eq!(13, R13.int());
        assert_eq!(14, R14.int());
        assert_eq!(15, R15.int());
    }

    #[test]
    fn test_msb() {
        assert_eq!(0, RAX.msb());
        assert_eq!(0, RCX.msb());
        assert_eq!(0, RDX.msb());
        assert_eq!(0, RBX.msb());
        assert_eq!(0, RSP.msb());
        assert_eq!(0, RBP.msb());
        assert_eq!(0, RSI.msb());
        assert_eq!(0, RDI.msb());
        assert_eq!(1, R8.msb());
        assert_eq!(1, R9.msb());
        assert_eq!(1, R10.msb());
        assert_eq!(1, R11.msb());
        assert_eq!(1, R12.msb());
        assert_eq!(1, R13.msb());
        assert_eq!(1, R14.msb());
        assert_eq!(1, R15.msb());
    }

    #[test]
    fn test_and7() {
        assert_eq!(0, RAX.and7());
        assert_eq!(1, RCX.and7());
        assert_eq!(2, RDX.and7());
        assert_eq!(3, RBX.and7());
        assert_eq!(4, RSP.and7());
        assert_eq!(5, RBP.and7());
        assert_eq!(6, RSI.and7());
        assert_eq!(7, RDI.and7());
        assert_eq!(0, R8.and7());
        assert_eq!(1, R9.and7());
        assert_eq!(2, R10.and7());
        assert_eq!(3, R11.and7());
        assert_eq!(4, R12.and7());
        assert_eq!(5, R13.and7());
        assert_eq!(6, R14.and7());
        assert_eq!(7, R15.and7());
    }
}
