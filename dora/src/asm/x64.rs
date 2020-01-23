use crate::asm::{Assembler, Register};

impl Register {
    fn low_bits(self) -> u8 {
        self.0 & 0b111
    }

    fn needs_rex(self) -> bool {
        self.0 > 7
    }
}

pub const RAX: Register = Register(0);
pub const RCX: Register = Register(1);
pub const RDX: Register = Register(2);
pub const RBX: Register = Register(3);
pub const RSP: Register = Register(4);
pub const RBP: Register = Register(5);
pub const RSI: Register = Register(6);
pub const RDI: Register = Register(7);

pub const R8: Register = Register(8);
pub const R9: Register = Register(9);
pub const R10: Register = Register(10);
pub const R11: Register = Register(11);
pub const R12: Register = Register(12);
pub const R13: Register = Register(13);
pub const R14: Register = Register(14);
pub const R15: Register = Register(15);

impl Assembler {
    pub fn pushqr(&mut self, reg: Register) {
        self.emit_rex_optional(reg);
        self.emit_u8(0x50 + reg.low_bits());
    }

    pub fn popqr(&mut self, reg: Register) {
        self.emit_rex_optional(reg);
        self.emit_u8(0x58 + reg.low_bits());
    }

    pub fn retq(&mut self) {
        self.emit_u8(0xC3);
    }

    pub fn nop(&mut self) {
        self.emit_u8(0x90);
    }

    fn emit_rex_optional(&mut self, reg: Register) {
        if reg.needs_rex() {
            self.emit_rex(false, false, false, true);
        }
    }

    fn emit_rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        // w - 64-bit width
        // r - extension of modrm-reg field
        // x - extension of sib index field
        // b - extension of modrm-rm/sib base/opcode reg field
        let opcode = 0x40 | (w as u8) << 3 | (r as u8) << 2 | (x as u8) << 1 | b as u8;
        self.emit_u8(opcode);
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::*;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = Assembler::new();
            buf.$name();
            let expected = vec![$($expr,)*];

            assert_eq!(expected, buf.code());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            )
        ) => {{
            let mut buf = Assembler::new();
            buf.$name($($param,)*);
            let expected = vec![$($expr,)*];
            let data = buf.code();

            if expected != data {
                print!("exp: ");

                for (ind, val) in expected.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                print!("\ngot: ");

                for (ind, val) in data.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                println!("");

                panic!("emitted code wrong.");
            }
        }};
    }

    #[test]
    fn test_popqr() {
        assert_emit!(0x58; popqr(RAX));
        assert_emit!(0x5c; popqr(RSP));
        assert_emit!(0x41, 0x58; popqr(R8));
        assert_emit!(0x41, 0x5F; popqr(R15));
    }

    #[test]
    fn test_pushqr() {
        assert_emit!(0x50; pushqr(RAX));
        assert_emit!(0x54; pushqr(RSP));
        assert_emit!(0x41, 0x50; pushqr(R8));
        assert_emit!(0x41, 0x57; pushqr(R15));
    }

    #[test]
    fn test_retq() {
        assert_emit!(0xc3; retq);
    }

    #[test]
    fn test_nop() {
        assert_emit!(0x90; nop);
    }
}
