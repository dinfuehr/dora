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
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0x50 + reg.low_bits());
    }

    pub fn popq_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0x58 + reg.low_bits());
    }

    pub fn retq(&mut self) {
        self.emit_u8(0xC3);
    }

    pub fn nop(&mut self) {
        self.emit_u8(0x90);
    }

    pub fn setcc_r(&mut self, condition: Condition, dest: Register) {
        if dest.needs_rex() || dest.low_bits() > 3 {
            self.emit_rex(false, false, false, dest.needs_rex());
        }

        self.emit_u8(0x0F);
        self.emit_u8((0x90 + condition.int()) as u8);
        self.emit_modrm_opcode(0, dest);
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

    pub fn subq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x29);
        self.emit_modrm_registers(src, dest);
    }

    pub fn subl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x29);
        self.emit_modrm_registers(src, dest);
    }

    pub fn andl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x21);
        self.emit_modrm_registers(src, dest);
    }

    pub fn andq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x21);
        self.emit_modrm_registers(src, dest);
    }

    pub fn cmpl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x39);
        self.emit_modrm_registers(src, dest);
    }

    pub fn cmpq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x39);
        self.emit_modrm_registers(src, dest);
    }

    pub fn orl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x09);
        self.emit_modrm_registers(src, dest);
    }

    pub fn orq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x09);
        self.emit_modrm_registers(src, dest);
    }

    pub fn xorl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x31);
        self.emit_modrm_registers(src, dest);
    }

    pub fn xorq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x31);
        self.emit_modrm_registers(src, dest);
    }

    pub fn testl_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex32_optional(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_modrm_registers(rhs, lhs);
    }

    pub fn testq_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex64_modrm(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_modrm_registers(rhs, lhs);
    }

    pub fn imull_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8(0xAF);
        self.emit_modrm_registers(dest, src);
    }

    pub fn imulq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8(0xAF);
        self.emit_modrm_registers(dest, src);
    }

    pub fn idivl_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xF7);
        self.emit_modrm_opcode(0b111, reg);
    }

    pub fn idivq_r(&mut self, src: Register) {
        self.emit_rex64_rm(src);
        self.emit_u8(0xF7);
        self.emit_modrm_opcode(0b111, src);
    }

    pub fn call_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xFF);
        self.emit_modrm_opcode(0b010, reg);
    }

    pub fn cdq(&mut self) {
        self.emit_u8(0x99);
    }

    pub fn cqo(&mut self) {
        self.emit_rex64();
        self.emit_u8(0x99);
    }

    fn emit_rex32_rm_optional(&mut self, reg: Register) {
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

    fn emit_rex64_rm(&mut self, rm: Register) {
        self.emit_rex(true, false, false, rm.needs_rex());
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

    fn emit_modrm_opcode(&mut self, opcode: u8, reg: Register) {
        self.emit_modrm(0b11, opcode, reg.low_bits());
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

#[derive(Copy, Clone)]
pub enum Condition {
    Overflow,
    NoOverflow,
    Below,
    NeitherAboveNorEqual,
    NotBelow,
    AboveOrEqual,
    Equal,
    Zero,
    NotEqual,
    NotZero,
    BelowOrEqual,
    NotAbove,
    NeitherBelowNorEqual,
    Above,
    Sign,
    NoSign,
    Parity,
    ParityEven,
    NoParity,
    ParityOdd,
    Less,
    NeitherGreaterNorEqual,
    NotLess,
    GreaterOrEqual,
    LessOrEqual,
    NotGreater,
    NeitherLessNorEqual,
    Greater,
}

impl Condition {
    pub fn int(self) -> i32 {
        match self {
            Condition::Overflow => 0b0000,
            Condition::NoOverflow => 0b0001,
            Condition::Below | Condition::NeitherAboveNorEqual => 0b0010,
            Condition::NotBelow | Condition::AboveOrEqual => 0b0011,
            Condition::Equal | Condition::Zero => 0b0100,
            Condition::NotEqual | Condition::NotZero => 0b0101,
            Condition::BelowOrEqual | Condition::NotAbove => 0b0110,
            Condition::NeitherBelowNorEqual | Condition::Above => 0b0111,
            Condition::Sign => 0b1000,
            Condition::NoSign => 0b1001,
            Condition::Parity | Condition::ParityEven => 0b1010,
            Condition::NoParity | Condition::ParityOdd => 0b1011,
            Condition::Less | Condition::NeitherGreaterNorEqual => 0b1100,
            Condition::NotLess | Condition::GreaterOrEqual => 0b1101,
            Condition::LessOrEqual | Condition::NotGreater => 0b1110,
            Condition::NeitherLessNorEqual | Condition::Greater => 0b1111,
        }
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

    #[test]
    fn test_setcc_r() {
        assert_emit!(0x0f, 0x94, 0xc0; setcc_r(Condition::Equal, RAX));
        assert_emit!(0x41, 0x0f, 0x95, 0xc7; setcc_r(Condition::NotEqual, R15));
        assert_emit!(0x0f, 0x9d, 0xc1; setcc_r(Condition::GreaterOrEqual, RCX));
        assert_emit!(0x0f, 0x9f, 0xc2; setcc_r(Condition::Greater, RDX));
        assert_emit!(0x40, 0x0f, 0x9e, 0xc6; setcc_r(Condition::LessOrEqual, RSI));
        assert_emit!(0x40, 0x0f, 0x9c, 0xc7; setcc_r(Condition::Less, RDI));
    }

    #[test]
    fn test_xorl_rr() {
        assert_emit!(0x44, 0x31, 0xf8; xorl_rr(RAX, R15));
        assert_emit!(0x31, 0xc8; xorl_rr(RAX, RCX));
        assert_emit!(0x41, 0x31, 0xc7; xorl_rr(R15, RAX));
    }

    #[test]
    fn test_xorq_rr() {
        assert_emit!(0x4C, 0x31, 0xf8; xorq_rr(RAX, R15));
        assert_emit!(0x48, 0x31, 0xc8; xorq_rr(RAX, RCX));
        assert_emit!(0x49, 0x31, 0xc7; xorq_rr(R15, RAX));
    }

    #[test]
    fn test_testl_rr() {
        assert_emit!(0x85, 0xc0; testl_rr(RAX, RAX));
        assert_emit!(0x85, 0xc6; testl_rr(RSI, RAX));
        assert_emit!(0x41, 0x85, 0xc7; testl_rr(R15, RAX));
    }

    #[test]
    fn test_testq_rr() {
        assert_emit!(0x48, 0x85, 0xc0; testq_rr(RAX, RAX));
        assert_emit!(0x48, 0x85, 0xc6; testq_rr(RSI, RAX));
        assert_emit!(0x49, 0x85, 0xc7; testq_rr(R15, RAX));
    }

    #[test]
    fn test_subl_rr() {
        assert_emit!(0x29, 0xd8; subl_rr(RAX, RBX));
        assert_emit!(0x44, 0x29, 0xf9; subl_rr(RCX, R15));
    }

    #[test]
    fn test_subq_rr() {
        assert_emit!(0x48, 0x29, 0xd8; subq_rr(RAX, RBX));
        assert_emit!(0x4c, 0x29, 0xf9; subq_rr(RCX, R15));
    }

    #[test]
    fn test_andl_rr() {
        assert_emit!(0x44, 0x21, 0xf8; andl_rr(RAX, R15));
        assert_emit!(0x21, 0xc8; andl_rr(RAX, RCX));
        assert_emit!(0x41, 0x21, 0xc7; andl_rr(R15, RAX));
    }

    #[test]
    fn test_andq_rr() {
        assert_emit!(0x4C, 0x21, 0xf8; andq_rr(RAX, R15));
        assert_emit!(0x48, 0x21, 0xc8; andq_rr(RAX, RCX));
        assert_emit!(0x49, 0x21, 0xc7; andq_rr(R15, RAX));
    }

    #[test]
    fn test_orl_rr() {
        assert_emit!(0x44, 0x09, 0xf8; orl_rr(RAX, R15));
        assert_emit!(0x09, 0xc8; orl_rr(RAX, RCX));
        assert_emit!(0x41, 0x09, 0xc7; orl_rr(R15, RAX));
    }

    #[test]
    fn test_orq_rr() {
        assert_emit!(0x4c, 0x09, 0xf8; orq_rr(RAX, R15));
        assert_emit!(0x48, 0x09, 0xc8; orq_rr(RAX, RCX));
        assert_emit!(0x49, 0x09, 0xc7; orq_rr(R15, RAX));
    }

    #[test]
    fn test_cmpl_rr() {
        assert_emit!(0x44, 0x39, 0xf8; cmpl_rr(RAX, R15));
        assert_emit!(0x41, 0x39, 0xdf; cmpl_rr(R15, RBX));
        assert_emit!(0x39, 0xd8; cmpl_rr(RAX, RBX));
    }

    #[test]
    fn test_cmpq_rr() {
        assert_emit!(0x4C, 0x39, 0xf8; cmpq_rr(RAX, R15));
        assert_emit!(0x49, 0x39, 0xdf; cmpq_rr(R15, RBX));
        assert_emit!(0x48, 0x39, 0xd8; cmpq_rr(RAX, RBX));
    }

    #[test]
    fn test_imull_rr() {
        assert_emit!(0x0f, 0xaf, 0xc3; imull_rr(RAX, RBX));
        assert_emit!(0x41, 0x0f, 0xaf, 0xcf; imull_rr(RCX, R15));
    }

    #[test]
    fn test_imulq_rr() {
        assert_emit!(0x48, 0x0f, 0xaf, 0xc3; imulq_rr(RAX, RBX));
        assert_emit!(0x49, 0x0f, 0xaf, 0xcf; imulq_rr(RCX, R15));
    }

    #[test]
    fn test_idivl_r() {
        assert_emit!(0xf7, 0xf8; idivl_r(RAX));
        assert_emit!(0x41, 0xf7, 0xff; idivl_r(R15));
    }

    #[test]
    fn test_idivq_r() {
        assert_emit!(0x48, 0xf7, 0xf8; idivq_r(RAX));
        assert_emit!(0x49, 0xf7, 0xff; idivq_r(R15));
    }

    #[test]
    fn test_call_r() {
        assert_emit!(0xff, 0xd0; call_r(RAX));
        assert_emit!(0x41, 0xff, 0xd7; call_r(R15));
    }
}
