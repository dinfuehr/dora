#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Reg {
    RAX, RCX, RDX, RBX, RSI, RDI, RBP, RSP,
    R8, R9, R10, R11, R12, R13, R14, R15
}

impl Reg {
    pub fn int(self) -> u8 {
        self as u8
    }

    pub fn msb(self) -> u8 {
        (self.int() >> 3) & 0x01
    }

    pub fn and7(self) -> u8 {
        self.int() & 0x07
    }
}
