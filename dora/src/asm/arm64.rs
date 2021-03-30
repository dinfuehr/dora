use crate::asm::{Assembler, Label, Register};
use crate::cpu::Reg;

pub const R0: Register = Register(0);
pub const R1: Register = Register(1);
pub const R2: Register = Register(2);
pub const R3: Register = Register(3);
pub const R4: Register = Register(4);
pub const R5: Register = Register(5);
pub const R6: Register = Register(6);
pub const R7: Register = Register(7);
pub const R8: Register = Register(8);
pub const R9: Register = Register(9);
pub const R10: Register = Register(10);
pub const R11: Register = Register(11);
pub const R12: Register = Register(12);
pub const R13: Register = Register(13);
pub const R14: Register = Register(14);
pub const R15: Register = Register(15);
pub const R16: Register = Register(16);
pub const R17: Register = Register(17);
pub const R18: Register = Register(18);
pub const R19: Register = Register(19);
pub const R20: Register = Register(20);
pub const R21: Register = Register(21);
pub const R22: Register = Register(22);
pub const R23: Register = Register(23);
pub const R24: Register = Register(24);
pub const R25: Register = Register(25);
pub const R26: Register = Register(26);
pub const R27: Register = Register(27);
pub const R28: Register = Register(28);
pub const R29: Register = Register(29);
pub const R30: Register = Register(30);
pub const REG_LR: Register = R30;
pub const REG_ZERO: Register = Register(31);
pub const REG_SP: Register = Register(32);

impl Register {
    fn encoding(self) -> u32 {
        debug_assert!(self.is_gpr_or_zero());
        self.0 as u32
    }

    fn is_gpr(&self) -> bool {
        self.0 <= 30
    }

    fn is_gpr_or_zero(&self) -> bool {
        self.0 <= 31
    }

    pub fn to_reg(self) -> Reg {
        assert!(self.0 <= 30);
        Reg(self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatRegister(u8);

impl FloatRegister {
    pub fn new(value: u8) -> FloatRegister {
        assert!(value < 32);
        FloatRegister(value)
    }

    fn encoding(self) -> u32 {
        self.0 as u32
    }
}

pub const F0: FloatRegister = FloatRegister(0);
pub const F1: FloatRegister = FloatRegister(1);
pub const F2: FloatRegister = FloatRegister(2);
pub const F3: FloatRegister = FloatRegister(3);
pub const F4: FloatRegister = FloatRegister(4);
pub const F5: FloatRegister = FloatRegister(5);
pub const F6: FloatRegister = FloatRegister(6);
pub const F7: FloatRegister = FloatRegister(7);
pub const F8: FloatRegister = FloatRegister(8);
pub const F9: FloatRegister = FloatRegister(9);
pub const F10: FloatRegister = FloatRegister(10);
pub const F11: FloatRegister = FloatRegister(11);
pub const F12: FloatRegister = FloatRegister(12);
pub const F13: FloatRegister = FloatRegister(13);
pub const F14: FloatRegister = FloatRegister(14);
pub const F15: FloatRegister = FloatRegister(15);
pub const F16: FloatRegister = FloatRegister(16);
pub const F17: FloatRegister = FloatRegister(17);
pub const F18: FloatRegister = FloatRegister(18);
pub const F19: FloatRegister = FloatRegister(19);
pub const F20: FloatRegister = FloatRegister(20);
pub const F21: FloatRegister = FloatRegister(21);
pub const F22: FloatRegister = FloatRegister(22);
pub const F23: FloatRegister = FloatRegister(23);
pub const F24: FloatRegister = FloatRegister(24);
pub const F25: FloatRegister = FloatRegister(25);
pub const F26: FloatRegister = FloatRegister(26);
pub const F27: FloatRegister = FloatRegister(27);
pub const F28: FloatRegister = FloatRegister(28);
pub const F29: FloatRegister = FloatRegister(29);
pub const F30: FloatRegister = FloatRegister(30);
pub const F31: FloatRegister = FloatRegister(31);

pub(super) enum JumpKind {
    Unconditional,
    Conditional(Cond),
    NonZero(bool, Register),
}

impl Assembler {
    pub(super) fn resolve_jumps(&mut self) {
        let unresolved_jumps = std::mem::replace(&mut self.unresolved_jumps, Vec::new());

        for (pc, lbl, kind) in unresolved_jumps {
            if let Some(lbl_offset) = self.offset(lbl) {
                let distance: i32 = lbl_offset as i32 - pc as i32;
                assert!(distance % 4 == 0);
                let distance = distance / 4;

                match kind {
                    JumpKind::Conditional(cond) => {
                        self.patch_u32(pc, inst_b_cond_imm(cond.into(), distance));
                    }

                    JumpKind::Unconditional => {
                        self.patch_u32(pc, inst_b_i(distance));
                    }

                    JumpKind::NonZero(sf, rt) => {
                        let sf = if sf { 1 } else { 0 };
                        self.patch_u32(pc, inst_cbnz(sf, rt.to_reg(), distance));
                    }
                }
            } else {
                panic!("unbound label");
            }
        }
    }

    pub fn asrv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b1010, rn, rd));
    }

    pub fn asrvw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b1010, rn, rd));
    }

    pub fn b_i(&mut self, imm26: i32) {
        self.emit_u32(inst_b_i(imm26));
    }

    pub fn b_l(&mut self, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.b_i(diff / 4);
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::Unconditional));
            }
        }
    }

    pub fn b_r(&mut self, rn: Register) {
        self.emit_u32(cls_uncond_branch_reg(0b0000, 0b11111, 0, rn, 0));
    }

    pub fn bc_l(&mut self, cond: Cond, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(inst_b_cond_imm(cond.into(), diff / 4));
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::Conditional(cond)));
            }
        }
    }

    pub fn bl_i(&mut self, imm26: i32) {
        self.emit_u32(cls_uncond_branch_imm(1, imm26));
    }

    pub fn bl_r(&mut self, rn: Register) {
        self.emit_u32(cls_uncond_branch_reg(0b0001, 0b11111, 0, rn, 0));
    }

    pub fn brk(&mut self, imm16: u32) {
        self.emit_u32(cls_exception(0b001, imm16, 0, 0));
    }

    pub fn cbnzx(&mut self, reg: Register, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(inst_cbnz(1, Reg(reg.encoding() as u8), diff / 4));
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::NonZero(true, reg)));
            }
        }
    }

    pub fn clsw(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(0, 0, 0b00000, 0b000101, rn, rd));
    }

    pub fn cls(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(1, 0, 0b00000, 0b000101, rn, rd));
    }

    pub fn clzw(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(0, 0, 0b00000, 0b000100, rn, rd));
    }

    pub fn clz(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(1, 0, 0b00000, 0b000100, rn, rd));
    }

    pub fn fcmp(&mut self, ty: u32, rn: FloatRegister, rm: FloatRegister) {
        self.emit_u32(cls_fp_compare(0, 0, ty, rm, 0, rn, 0));
    }

    pub fn fcmpe(&mut self, ty: u32, rn: FloatRegister, rm: FloatRegister) {
        self.emit_u32(cls_fp_compare(0, 0, ty, rm, 0, rn, 0b10000));
    }

    pub fn lslv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b1000, rn, rd));
    }

    pub fn lslvw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b1000, rn, rd));
    }

    pub fn lsrv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b1001, rn, rd));
    }

    pub fn lsrvw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b1001, rn, rd));
    }

    pub fn nop(&mut self) {
        self.emit_u32(cls_system(0));
    }

    pub fn rbitw(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(0, 0, 0b00000, 0b000000, rn, rd));
    }

    pub fn rbit(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(1, 0, 0b00000, 0b000000, rn, rd));
    }

    pub fn ret(&mut self, rn: Register) {
        self.emit_u32(cls_uncond_branch_reg(0b0010, 0b11111, 0, rn, 0));
    }

    pub fn revw(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(0, 0, 0b00000, 0b000001, rn, rd));
    }

    pub fn rev(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls_dataproc1(0, 0, 0b00000, 0b000001, rn, rd));
    }

    pub fn rorv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b1011, rn, rd));
    }

    pub fn rorvw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b1011, rn, rd));
    }

    pub fn sdiv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b11, rn, rd));
    }

    pub fn sdivw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b11, rn, rd));
    }

    pub fn udiv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(1, 0, rm, 0b10, rn, rd));
    }

    pub fn udivw(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls_dataproc2(0, 0, rm, 0b10, rn, rd));
    }
}

fn inst_b_i(imm26: i32) -> u32 {
    cls_uncond_branch_imm(0, imm26)
}

fn inst_cbz(sf: u32, rt: Reg, imm19: i32) -> u32 {
    cls_cmp_branch_imm(sf, 0b0, rt, imm19)
}

fn inst_cbnz(sf: u32, rt: Reg, imm19: i32) -> u32 {
    cls_cmp_branch_imm(sf, 0b1, rt, imm19)
}

fn cls_cmp_branch_imm(sf: u32, op: u32, rt: Reg, imm19: i32) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_i19(imm19));
    assert!(rt.is_gpr());
    let imm = (imm19 as u32) & 0x7FFFF;

    sf << 31 | 0b011010u32 << 25 | op << 24 | imm << 5 | rt.asm()
}

fn cls_dataproc1(sf: u32, s: u32, opcode2: u32, opcode: u32, rn: Register, rd: Register) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(sf));
    assert!(fits_u5(opcode2));
    assert!(fits_u6(opcode));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31
        | 1 << 30
        | s << 29
        | 0b11010110 << 21
        | opcode2 << 16
        | opcode << 10
        | rn.encoding() << 5
        | rd.encoding()
}

fn cls_exception(opc: u32, imm16: u32, op2: u32, ll: u32) -> u32 {
    assert!(fits_u3(opc));
    assert!(fits_u16(imm16));
    assert!(op2 == 0);
    assert!(fits_u2(ll));

    0b11010100u32 << 24 | opc << 21 | imm16 << 5 | op2 << 2 | ll
}

fn cls_ldst_exclusive(
    _size: u32,
    _o2: u32,
    _l: u32,
    _o1: u32,
    _rs: Register,
    _o0: u32,
    _rt2: Register,
    _rn: Register,
    _rt: Register,
) -> u32 {
    unimplemented!()
}

pub fn inst_b_cond_imm(cond: Cond, imm19: i32) -> u32 {
    cls_cond_branch_imm(cond, imm19)
}

fn cls_cond_branch_imm(cond: Cond, imm19: i32) -> u32 {
    assert!(fits_i19(imm19));

    let imm = (imm19 as u32) & 0x7FFFF;

    0b01010100u32 << 24 | imm << 5 | cond.u32()
}

fn cls_uncond_branch_imm(op: u32, imm26: i32) -> u32 {
    assert!(fits_bit(op));
    assert!(fits_i26(imm26));

    0b101u32 << 26 | op << 31 | ((imm26 as u32) & 0x3FFFFFF)
}

fn cls_uncond_branch_reg(opc: u32, op2: u32, op3: u32, rn: Register, op4: u32) -> u32 {
    assert!(fits_u4(opc));
    assert!(fits_u5(op2));
    assert!(fits_u6(op3));
    assert!(fits_u5(op4));

    (0b1101011 as u32) << 25 | opc << 21 | op2 << 16 | op3 << 10 | encoding_rn(rn) | op4
}

fn cls_system(imm: u32) -> u32 {
    assert!(fits_u7(imm));

    0xD503201F | imm << 5
}

fn cls_dataproc2(sf: u32, s: u32, rm: Register, opcode: u32, rn: Register, rd: Register) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(s));
    assert!(rm.is_gpr());
    assert!(fits_u6(opcode));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31
        | s << 29
        | 0b11010110u32 << 21
        | rm.encoding() << 16
        | opcode << 10
        | rn.encoding() << 5
        | rd.encoding()
}

fn cls_fp_compare(
    m: u32,
    s: u32,
    ty: u32,
    rm: FloatRegister,
    op: u32,
    rn: FloatRegister,
    opcode2: u32,
) -> u32 {
    assert!(m == 0);
    assert!(s == 0);
    assert!(fits_bit(ty));
    assert!(fits_u2(op));
    assert!(fits_u5(opcode2));

    m << 31
        | s << 29
        | 0b11110 << 24
        | ty << 22
        | 1 << 21
        | rm.encoding() << 16
        | op << 14
        | 0b1000 << 10
        | rn.encoding() << 5
        | opcode2
}

fn encoding_rn(reg: Register) -> u32 {
    assert!(reg.is_gpr());
    reg.encoding() << 5
}

fn fits_bit(imm: u32) -> bool {
    imm < (1 << 1)
}

fn fits_i19(imm: i32) -> bool {
    -262_144 <= imm && imm < 262_144
}

fn fits_i26(imm: i32) -> bool {
    -(1 << 25) <= imm && imm < (1 << 25)
}

fn fits_u2(imm: u32) -> bool {
    imm < 4
}

fn fits_u3(imm: u32) -> bool {
    imm < 8
}

fn fits_u4(imm: u32) -> bool {
    imm < (1 << 4)
}

fn fits_u5(imm: u32) -> bool {
    imm < (1 << 5)
}

fn fits_u6(imm: u32) -> bool {
    imm < (1 << 6)
}

fn fits_u7(imm: u32) -> bool {
    imm < (1 << 7)
}

fn fits_u16(imm: u32) -> bool {
    imm < (1 << 16)
}
#[derive(Copy, Clone)]
pub enum Cond {
    EQ, // equal
    NE, // not equal
    CS,
    HS, // carry set, unsigned higher or same
    CC,
    LO, // carry clear, unsigned lower
    MI, // negative
    PL, // positive or zero
    VS, // overflow
    VC, // no overflow
    HI, // unsigned higher
    LS, // unsigned lower or same
    GE, // signed greater than or equal
    LT, // signed less than
    GT, // signed greater than
    LE, // signed less than or equal
}

impl Cond {
    pub fn invert(self) -> Cond {
        match self {
            Cond::EQ => Cond::NE,
            Cond::NE => Cond::EQ,
            Cond::CS | Cond::HS => Cond::CC,
            Cond::CC | Cond::LO => Cond::CS,
            Cond::MI => Cond::PL,
            Cond::PL => Cond::MI,
            Cond::VS => Cond::VC,
            Cond::VC => Cond::VS,
            Cond::HI => Cond::LS,
            Cond::LS => Cond::HI,
            Cond::GE => Cond::LT,
            Cond::LT => Cond::GE,
            Cond::GT => Cond::LE,
            Cond::LE => Cond::GT,
        }
    }

    pub fn u32(self) -> u32 {
        match self {
            Cond::EQ => 0b0000,
            Cond::NE => 0b0001,
            Cond::CS | Cond::HS => 0b0010,
            Cond::CC | Cond::LO => 0b0011,
            Cond::MI => 0b0100,
            Cond::PL => 0b0101,
            Cond::VS => 0b0110,
            Cond::VC => 0b0111,
            Cond::HI => 0b1000,
            Cond::LS => 0b1001,
            Cond::GE => 0b1010,
            Cond::LT => 0b1011,
            Cond::GT => 0b1100,
            Cond::LE => 0b1101,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::*;
    use byteorder::{LittleEndian, WriteBytesExt};

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),*
            )
        ) => {{
            let mut buf = Assembler::new();
            buf.$name($($param,)*);
            let mut expected: Vec<u8> = Vec::new();
            $(
                expected.write_u32::<LittleEndian>($expr).unwrap();
            )*
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
    fn test_b_i() {
        assert_emit!(0x14000000; b_i(0));
        assert_emit!(0x17FFFFFF; b_i(-1));
        assert_emit!(0x14000001; b_i(1));
    }

    #[test]
    fn test_bl_i() {
        assert_emit!(0x94000000; bl_i(0));
        assert_emit!(0x97FFFFFF; bl_i(-1));
        assert_emit!(0x94000001; bl_i(1));
    }

    #[test]
    fn test_br_bl_r() {
        assert_emit!(0xd61f0000; b_r(R0));
        assert_emit!(0xd61f03c0; b_r(R30));
        assert_emit!(0xd63f0000; bl_r(R0));
        assert_emit!(0xd63f03c0; bl_r(R30));
    }

    #[test]
    fn test_brk() {
        assert_emit!(0xd4200000; brk(0));
        assert_emit!(0xd43fffe0; brk(0xFFFF));
    }

    #[test]
    fn test_fcmp() {
        assert_emit!(0x1e212000; fcmp(0, F0, F1));
        assert_emit!(0x1e612000; fcmp(1, F0, F1));
        assert_emit!(0x1e252080; fcmp(0, F4, F5));
    }

    #[test]
    fn test_fcmpe() {
        assert_emit!(0x1e212010; fcmpe(0, F0, F1));
        assert_emit!(0x1e612010; fcmpe(1, F0, F1));
        assert_emit!(0x1e252090; fcmpe(0, F4, F5));
    }

    #[test]
    fn test_ret() {
        assert_emit!(0xd65f03c0; ret(REG_LR));
        assert_emit!(0xd65f0000; ret(R0));
        assert_emit!(0xd65f0140; ret(R10));
    }

    #[test]
    fn test_nop() {
        assert_emit!(0xd503201f; nop());
    }

    #[test]
    fn test_div() {
        assert_emit!(0x1ac20820; udivw(R0, R1, R2));
        assert_emit!(0x9ac50c83; sdiv(R3, R4, R5));
        assert_emit!(0x1ac820e6; lslvw(R6, R7, R8));
        assert_emit!(0x1acb2549; lsrvw(R9, R10, R11));
        assert_emit!(0x1ace29ac; asrvw(R12, R13, R14));
        assert_emit!(0x1ad12e0f; rorvw(R15, R16, R17));
    }

    #[test]
    fn test_b_cond_imm() {
        assert_eq!(0x54ffffe0, inst_b_cond_imm(Cond::EQ, -1));
        assert_eq!(0x54ffffc1, inst_b_cond_imm(Cond::NE, -2));
        assert_eq!(0x54000044, inst_b_cond_imm(Cond::MI, 2));
        assert_eq!(0x5400002b, inst_b_cond_imm(Cond::LT, 1));
    }
}
