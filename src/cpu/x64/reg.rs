use cpu::Reg;

pub const REG_COUNT: usize = 16;
pub static REG_PARAMS: [Reg; 6] = [RDI, RSI, RDX, RCX, R8, R9];
pub static SCRATCH: [Reg; 1] = [R9];

pub const REG_RESULT: Reg = RAX;
pub const REG_TMP1: Reg = R10;
pub const REG_TMP2: Reg = R11;
pub const REG_SP: Reg = RSP;
pub const REG_FP: Reg = RBP;

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
