pub const INST_RET: u8 = 0xC3;
pub const INST_NOP: u8 = 0x90;

#[derive(Copy, Clone, Debug)]
pub enum Reg32 {
    EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP,
    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D
}

impl Reg32 {
    pub fn and7(self) -> u8 {
        (self as u8) & 0x07
    }
}
