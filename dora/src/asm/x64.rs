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
    pub fn pushq_r(&mut self, reg: Register) {
        self.emit_rex_optional(reg);
        self.emit_u8(0x50 + reg.low_bits());
    }

    pub fn popq_r(&mut self, reg: Register) {
        self.emit_rex_optional(reg);
        self.emit_u8(0x58 + reg.low_bits());
    }

    pub fn retq(&mut self) {
        self.emit_u8(0xC3);
    }

    pub fn nop(&mut self) {
        self.emit_u8(0x90);
    }

    pub fn movq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x89);
        self.emit_modrm_registers(src, dest);
    }

    pub fn movl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x89);
        self.emit_modrm_registers(src, dest);
    }

    pub fn addq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x01);
        self.emit_modrm_registers(src, dest);
    }

    pub fn addl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x01);
        self.emit_modrm_registers(src, dest);
    }

    pub fn cdq(&mut self) {
        self.emit_u8(0x99);
    }

    pub fn cqo(&mut self) {
        self.emit_rex64();
        self.emit_u8(0x99);
    }

    fn emit_rex_optional(&mut self, reg: Register) {
        if reg.needs_rex() {
            self.emit_rex(false, false, false, true);
        }
    }

    fn emit_rex32_optional(&mut self, reg: Register, rm: Register) {
        if reg.needs_rex() || rm.needs_rex() {
            self.emit_rex(false, reg.needs_rex(), false, rm.needs_rex());
        }
    }

    fn emit_rex64(&mut self) {
        self.emit_rex(true, false, false, false);
    }

    fn emit_rex64_modrm(&mut self, reg: Register, rm: Register) {
        self.emit_rex(true, reg.needs_rex(), false, rm.needs_rex());
    }

    fn emit_rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        // w - 64-bit width
        // r - extension of modrm-reg field
        // x - extension of sib index field
        // b - extension of modrm-rm/sib base/opcode reg field
        let opcode = 0x40 | (w as u8) << 3 | (r as u8) << 2 | (x as u8) << 1 | b as u8;
        self.emit_u8(opcode);
    }

    fn emit_modrm_registers(&mut self, reg: Register, rm: Register) {
        self.emit_modrm(0b11, reg.low_bits(), rm.low_bits());
    }

    fn emit_modrm(&mut self, mode: u8, reg: u8, rm: u8) {
        assert!(mode < 4);
        assert!(reg < 8);
        assert!(rm < 8);
        self.emit_u8(mode << 6 | reg << 3 | rm);
    }

    fn emit_sib(&mut self, scale: u8, index: u8, base: u8) {
        assert!(scale < 4);
        assert!(index < 8);
        assert!(base < 8);
        self.emit_u8(scale << 6 | index << 3 | base);
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
    fn test_popq_r() {
        assert_emit!(0x58; popq_r(RAX));
        assert_emit!(0x5c; popq_r(RSP));
        assert_emit!(0x41, 0x58; popq_r(R8));
        assert_emit!(0x41, 0x5F; popq_r(R15));
    }

    #[test]
    fn test_pushq_r() {
        assert_emit!(0x50; pushq_r(RAX));
        assert_emit!(0x54; pushq_r(RSP));
        assert_emit!(0x41, 0x50; pushq_r(R8));
        assert_emit!(0x41, 0x57; pushq_r(R15));
    }

    #[test]
    fn test_retq() {
        assert_emit!(0xc3; retq);
    }

    #[test]
    fn test_nop() {
        assert_emit!(0x90; nop);
    }

    #[test]
    fn test_emit_movq_rr() {
        assert_emit!(0x49, 0x89, 0xc7; movq_rr(R15, RAX));
        assert_emit!(0x4c, 0x89, 0xf8; movq_rr(RAX, R15));
        assert_emit!(0x48, 0x89, 0xe5; movq_rr(RBP, RSP));
        assert_emit!(0x48, 0x89, 0xec; movq_rr(RSP, RBP));
    }

    #[test]
    fn test_emit_movl_rr() {
        assert_emit!(0x41, 0x89, 0xc7; movl_rr(R15, RAX));
        assert_emit!(0x44, 0x89, 0xf8; movl_rr(RAX, R15));
        assert_emit!(0x89, 0xc1; movl_rr(RCX, RAX));
    }

    #[test]
    fn test_emit_addq_rr() {
        assert_emit!(0x48, 0x01, 0xD8; addq_rr(RAX, RBX));
        assert_emit!(0x4C, 0x01, 0xE0; addq_rr(RAX, R12));
        assert_emit!(0x49, 0x01, 0xC4; addq_rr(R12, RAX));
        assert_emit!(0x49, 0x01, 0xE7; addq_rr(R15, RSP));
    }

    #[test]
    fn test_emit_addl_rr() {
        assert_emit!(0x01, 0xd8; addl_rr(RAX, RBX));
        assert_emit!(0x44, 0x01, 0xf9; addl_rr(RCX, R15));
    }

    #[test]
    fn test_cdq_cqo() {
        assert_emit!(0x99; cdq);
        assert_emit!(0x48, 0x99; cqo);
    }
}
