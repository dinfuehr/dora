use crate::asm::{Assembler, Label, Register};
use crate::cpu::arm64::asm;
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
    fn value(self) -> u32 {
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

pub(super) enum JumpKind {
    Unconditional,
    Conditional(asm::Cond),
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
                        self.patch_u32(pc, asm::b_cond_imm(cond.into(), distance));
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

    pub fn bc_l(&mut self, cond: asm::Cond, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(asm::b_cond_imm(cond.into(), diff / 4));
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
                self.emit_u32(inst_cbnz(1, Reg(reg.value() as u8), diff / 4));
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
        | rn.value() << 5
        | rd.value()
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
        | rm.value() << 16
        | opcode << 10
        | rn.value() << 5
        | rd.value()
}

fn encoding_rn(reg: Register) -> u32 {
    assert!(reg.is_gpr());
    reg.value() << 5
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
}
