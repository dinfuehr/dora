use cpu::Reg;
use cpu::arm64::reg::*;
use jit::buffer::Buffer;

pub fn ret() -> u32 {
    cls_uncond_branch_reg(0b0010, 0b11111, 0, REG_LR, 0)
}

pub fn ret_reg(rn: Reg) -> u32 {
    cls_uncond_branch_reg(0b0010, 0b11111, 0, rn, 0)
}

pub fn br(rn: Reg) -> u32 {
    cls_uncond_branch_reg(0b0000, 0b11111, 0, rn, 0)
}

pub fn blr(rn: Reg) -> u32 {
    cls_uncond_branch_reg(0b0001, 0b11111, 0, rn, 0)
}

fn cls_uncond_branch_reg(opc: u32, op2: u32, op3: u32, rn: Reg, op4: u32) -> u32 {
    assert!(fits_u4(opc));
    assert!(fits_u5(op2));
    assert!(fits_u6(op3));
    assert!(rn.is_gpr());
    assert!(fits_u5(op4));

    (0b1101011 as u32) << 25 | opc << 21 | op2 << 16 |
        op3 << 10 | rn.u32() << 5 | op4
}

pub fn b_imm(imm26: i32) -> u32 {
    cls_uncond_branch_imm(0, imm26)
}

pub fn bl_imm(imm26: i32) -> u32 {
    cls_uncond_branch_imm(1, imm26)
}

fn cls_uncond_branch_imm(op: u32, imm26: i32) -> u32 {
    assert!(fits_bit(op));
    assert!(fits_i26(imm26));

    0b101u32 << 26 | op << 31 | ((imm26 as u32) & 0x3FFFFFF)
}

pub fn b_cond_imm(cond: Cond, imm19: i32) -> u32 {
    cls_cond_branch_imm(cond, imm19)
}

fn cls_cond_branch_imm(cond: Cond, imm19: i32) -> u32 {
    assert!(fits_i19(imm19));

    let imm = (imm19 as u32) & 0x7FFFF;

    0b01010100u32 << 24 | imm << 5 | cond.u32()
}

pub fn nop() -> u32 {
    cls_system(0)
}

fn cls_system(imm: u32) -> u32 {
    assert!(fits_u7(imm));

    0xD503201F | imm << 5
}

pub fn add_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 0, 0, shift, imm12, rn, rd)
}

pub fn sub_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 1, 0, shift, imm12, rn, rd)
}

fn cls_addsub_imm(sf: u32, op: u32, s: u32, shift: u32, imm12: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(fits_bit(shift));
    assert!(fits_u12(imm12));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    (0b10001 as u32) << 24 | sf << 31 | op << 30 | s << 29 |
        shift << 22 | imm12 << 10 | rn.u32() << 5 | rd.u32()
}

pub fn add_reg(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_addsub_shreg(sf, 0, 0, Shift::LSL, rm, 0, rn, rd)
}

pub fn sub_reg(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_addsub_shreg(sf, 1, 0, Shift::LSL, rm, 0, rn, rd)
}

pub fn adds_reg(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_addsub_shreg(sf, 0, 1, Shift::LSL, rm, 0, rn, rd)
}

pub fn subs_reg(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_addsub_shreg(sf, 1, 1, Shift::LSL, rm, 0, rn, rd)
}

pub fn cmp_reg(sf: u32, rn: Reg, rm: Reg) -> u32 {
    subs_reg(sf, REG_ZERO, rn, rm)
}

pub fn add_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 0, 0, shift, rm, amount, rn, rd)
}

pub fn sub_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 1, 0, shift, rm, amount, rn, rd)
}

pub fn adds_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 0, 1, shift, rm, amount, rn, rd)
}

pub fn subs_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 1, 1, shift, rm, amount, rn, rd)
}

pub fn cmp_shreg(sf: u32, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 1, 1, shift, rm, amount, rn, REG_ZERO)
}

fn cls_addsub_shreg(sf: u32, op: u32, s: u32, shift: Shift, rm: Reg,
                    imm6: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(!shift.is_ror());
    assert!(rm.is_gpr());
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());
    assert!(fits_u5(imm6));

    0b01011u32 << 24 | sf << 31 | op << 30 | s << 29 |
        shift.u32() << 22 | rm.u32() << 16 | imm6 << 10 | rn.u32() << 5 | rd.u32()
}

pub fn ldrb_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b00, 0, 0b01, rm, extend, amount, rn, rt)
}

pub fn ldrh_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b01, 0, 0b01, rm, extend, amount, rn, rt)
}

pub fn ldrw_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b10, 0, 0b01, rm, extend, amount, rn, rt)
}

pub fn ldrx_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b11, 0, 0b01, rm, extend, amount, rn, rt)
}

pub fn strb_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b00, 0, 0b00, rm, extend, amount, rn, rt)
}

pub fn strh_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b01, 0, 0b00, rm, extend, amount, rn, rt)
}


pub fn strw_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b10, 0, 0b00, rm, extend, amount, rn, rt)
}

pub fn strx_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b11, 0, 0b00, rm, extend, amount, rn, rt)
}

fn cls_ldst_regoffset(size: u32, v: u32, opc: u32, rm: Reg, option: LdStExtend,
                      s: u32, rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(size));
    assert!(fits_bit(v));
    assert!(fits_u2(opc));
    assert!(rm.is_gpr());
    assert!(fits_bit(s));
    assert!(rn.is_gpr());
    assert!(rt.is_gpr());

    0b111u32 << 27 | 1u32 << 21 | 0b10u32 << 10 | size << 30 |
        v << 26 | opc << 22 | rm.u32() << 16 | option.u32() << 13 | s << 12 |
        rn.u32() << 5 | rt.u32()
}

pub fn ldrb_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b00, 0, 0b01, imm12, rn, rt)
}

pub fn ldrh_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b01, 0, 0b01, imm12, rn, rt)
}

pub fn ldrw_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b10, 0, 0b01, imm12, rn, rt)
}

pub fn ldrx_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b11, 0, 0b01, imm12, rn, rt)
}

pub fn strb_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b00, 0, 0b00, imm12, rn, rt)
}

pub fn strh_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b01, 0, 0b00, imm12, rn, rt)
}

pub fn strw_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b10, 0, 0b00, imm12, rn, rt)
}

pub fn strx_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b11, 0, 0b00, imm12, rn, rt)
}

fn cls_ldst_regimm(size: u32, v: u32, opc: u32, imm12: u32, rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(size));
    assert!(fits_bit(v));
    assert!(fits_u2(opc));
    assert!(fits_u12(imm12));
    assert!(rn.is_gpr());
    assert!(rt.is_gpr());

    0b111001u32 << 24 | size << 30 | v << 26 | opc << 22 |
        imm12 << 10 | rn.u32() << 5 | rt.u32()
}

pub fn ldrw_literal(rt: Reg, imm19: i32) -> u32 {
    cls_ld_literal(0b00, 0, imm19, rt)
}

pub fn ldrx_literal(rt: Reg, imm19: i32) -> u32 {
    cls_ld_literal(0b01, 0, imm19, rt)
}

fn cls_ld_literal(opc: u32, v: u32, imm19: i32, rt: Reg) -> u32 {
    assert!(fits_u2(opc));
    assert!(fits_bit(v));
    assert!(fits_i19(imm19));
    assert!(rt.is_gpr());

    let imm = (imm19 as u32) & 0x7FFFF;

    0b011u32 << 27 | opc << 30 | v << 26 | imm << 5 | rt.u32()
}

pub fn and_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b00, shift, 0, rm, imm6, rn, rd)
}

pub fn bic_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b00, shift, 1, rm, imm6, rn, rd)
}

pub fn orr_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b01, shift, 0, rm, imm6, rn, rd)
}

pub fn orn_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b01, shift, 1, rm, imm6, rn, rd)
}

pub fn eor_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b10, shift, 0, rm, imm6, rn, rd)
}

pub fn eon_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b10, shift, 1, rm, imm6, rn, rd)
}

pub fn ands_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b11, shift, 0, rm, imm6, rn, rd)
}

pub fn bics_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, imm6: u32) -> u32 {
    cls_logical_shreg(sf, 0b11, shift, 1, rm, imm6, rn, rd)
}

fn cls_logical_shreg(sf: u32, opc: u32, shift: Shift, n: u32, rm: Reg,
                     imm6: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_bit(n));
    assert!(rm.is_gpr());
    assert!(fits_u5(imm6));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    0b01010u32 << 24 | sf << 31 | opc << 29 | shift.u32() << 22 |
        n << 21 | rm.u32() << 16 | imm6 << 10 |
        rn.u32() << 5 | rd.u32()
}

pub fn brk(imm16: u32) -> u32 {
    cls_exception(0b001, imm16, 0, 0)
}

fn cls_exception(opc: u32, imm16: u32, op2: u32, ll: u32) -> u32 {
    assert!(fits_u3(opc));
    assert!(fits_u16(imm16));
    assert!(op2 == 0);
    assert!(fits_u2(ll));

    0b11010100u32 << 24 | opc << 21 | imm16 << 5 | op2 << 2 | ll
}

pub fn csel(sf: u32, rd: Reg, rn: Reg, rm: Reg, cond: Cond) -> u32 {
    cls_cond_select(sf, 0, 0, rm, cond, 0, rn, rd)
}

pub fn csinc(sf: u32, rd: Reg, rn: Reg, rm: Reg, cond: Cond) -> u32 {
    cls_cond_select(sf, 0, 0, rm, cond, 1, rn, rd)
}

pub fn cset(sf: u32, rd: Reg, cond: Cond) -> u32 {
    csinc(sf, rd, REG_ZERO, REG_ZERO, cond.invert())
}

fn cls_cond_select(sf: u32, op: u32, s: u32, rm: Reg, cond: Cond, op2: u32,
                   rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(rm.is_gpr());
    assert!(fits_bit(op2));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    0b11010100u32 << 21 | sf << 31 | op << 30 | s << 29 |
        rm.u32() << 16 | cond.u32() << 12 | op2 << 10 |
        rn.u32() << 5 | rd.u32()
}

fn movn(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b00, shift, imm16, rd)
}

fn movz(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b10, shift, imm16, rd)
}

fn movk(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b11, shift, imm16, rd)
}

fn cls_move_wide_imm(sf: u32, opc: u32, hw: u32, imm16: u32, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_bit(hw));
    assert!(fits_u16(imm16));
    assert!(rd.is_gpr());

    0b100101u32 << 23 | sf << 31 | opc << 29 | hw << 21 |
        imm16 << 5 | rd.u32()
}

pub fn adr(rd: Reg, imm: i32) -> u32 {
    cls_pcrel(0, imm, rd)
}

pub fn adrp(rd: Reg, imm: i32) -> u32 {
    cls_pcrel(1, imm, rd)
}

fn cls_pcrel(op: u32, imm: i32, rd: Reg) -> u32 {
    assert!(fits_i21(imm));
    assert!(fits_bit(op));

    let imm = imm as u32;

    let immlo = imm & 3;
    let immhi = (imm >> 2) & 0x7FFFF;

    1u32 << 28 | op << 31 | immlo << 29 | immhi << 5 | rd.u32()
}

pub fn udiv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b10, rn, rd)
}

pub fn sdiv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b11, rn, rd)
}

pub fn lslv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b1000, rn, rd)
}

pub fn lsrv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b1001, rn, rd)
}

pub fn asrv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b1010, rn, rd)
}

pub fn rorv(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_dataproc2(sf, 0, rm, 0b1011, rn, rd)
}

fn cls_dataproc2(sf: u32, s: u32, rm: Reg, opcode: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(s));
    assert!(fits_u6(opcode));
    assert!(rm.is_gpr());
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31 | s << 29 | 0b11010110u32 << 21 | rm.u32() << 16 |
        opcode << 10 | rn.u32() << 5 | rd.u32()
}

pub fn madd(sf: u32, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> u32 {
    cls_dataproc3(sf, 0, 0, rm, 0, ra, rn, rd)
}

pub fn msub(sf: u32, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> u32 {
    cls_dataproc3(sf, 0, 0, rm, 1, ra, rn, rd)
}

pub fn mul(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    madd(sf, rd, rn, rm, REG_ZERO)
}

fn cls_dataproc3(sf: u32, op54: u32, op31: u32, rm: Reg, o0: u32,
                 ra: Reg, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(op54));
    assert!(fits_u3(op31));
    assert!(fits_bit(o0));
    assert!(rm.is_gpr());
    assert!(ra.is_gpr());
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31 | op54 << 29 | 0b11011u32 << 24 | op31 << 21 |
        rm.u32() << 16 | o0 << 15 | ra.u32() << 10 | rn.u32() << 5 |
        rd.u32()
}

pub fn ldpw(rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    cls_ldst_pair(0b00, 0, 1, imm7, rt2, rn, rt)
}

pub fn stpw(rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    cls_ldst_pair(0b00, 0, 0, imm7, rt2, rn, rt)
}

pub fn ldpx(rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    cls_ldst_pair(0b10, 0, 1, imm7, rt2, rn, rt)
}

pub fn stpx(rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    cls_ldst_pair(0b10, 0, 0, imm7, rt2, rn, rt)
}

fn cls_ldst_pair(opc: u32, v: u32, l: u32, imm7: i32, rt2: Reg,
                 rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(opc));
    assert!(fits_bit(v));
    assert!(fits_bit(l));
    assert!(fits_i7(imm7));
    assert!(rt2.is_gpr());
    assert!(rn.is_gpr());
    assert!(rt.is_gpr());

    let imm = (imm7 as u32) & 0x7F;

    opc << 30 | 0b101u32 << 27 | 1u32 << 24 | l << 22 | imm << 15 |
        rt2.u32() << 10 | rn.u32() << 5 | rt.u32()
}

#[derive(Copy, Clone)]
pub enum Cond {
    EQ, // equal
    NE, // not equal
    CS, HS, // carry set, unsigned higher or same
    CC, LO, // carry clear, unsigned lower
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
    fn invert(self) -> Cond {
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

    fn u32(self) -> u32 {
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

#[derive(Copy, Clone)]
pub enum Extend {
    UXTB, UXTH, LSL, UXTW, UXTX,
    SXTB, SXTH, SXTW, SXTX,
}

impl Extend {
    fn is_ldr(self) -> bool {
        match self {
            Extend::UXTW |
                Extend::LSL |
                Extend::SXTW |
                Extend::SXTX => true,

            _ => false,
        }
    }

    fn u32(self) -> u32 {
        match self {
            Extend::UXTB => 0b000,
            Extend::UXTH => 0b001,
            Extend::LSL  => 0b010,
            Extend::UXTW => 0b010,
            Extend::UXTX => 0b011,
            Extend::SXTB => 0b100,
            Extend::SXTH => 0b101,
            Extend::SXTW => 0b110,
            Extend::SXTX => 0b111,
        }
    }
}

#[derive(Copy, Clone)]
pub enum LdStExtend {
    UXTW, LSL, SXTW, SXTX
}

impl LdStExtend {
    fn u32(self) -> u32 {
        match self {
            LdStExtend::UXTW => 0b010,
            LdStExtend::LSL  => 0b011,
            LdStExtend::SXTW => 0b110,
            LdStExtend::SXTX => 0b111,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Shift {
    LSL, LSR, ASR, ROR
}

impl Shift {
    fn is_ror(self) -> bool {
        match self {
            Shift::ROR => true,
            _ => false,
        }
    }

    fn u32(self) -> u32 {
        match self {
            Shift::LSL => 0,
            Shift::LSR => 1,
            Shift::ASR => 2,
            Shift::ROR => 3,
        }
    }
}

fn fits_bit(imm: u32) -> bool {
    imm < 2
}

fn fits_u2(imm: u32) -> bool {
    imm < 4
}

fn fits_u3(imm: u32) -> bool {
    imm < 8
}

fn fits_u4(imm: u32) -> bool {
    imm < 16
}

fn fits_u5(imm: u32) -> bool {
    imm < 32
}

fn fits_u6(imm: u32) -> bool {
    imm < 64
}

fn fits_u7(imm: u32) -> bool {
    imm < 128
}

fn fits_i7(imm: i32) -> bool {
    -64 <= imm && imm < 64
}

fn fits_u12(imm: u32) -> bool {
    imm < 4096
}

fn fits_i12(imm: i32) -> bool {
    -2048 <= imm && imm < 2048
}

fn fits_u16(imm: u32) -> bool {
    imm < 65_536
}

fn fits_i19(imm: i32) -> bool {
    -262_144 <= imm && imm < 262_144
}

fn fits_i21(imm: i32) -> bool {
    -(2 << 20) <= imm && imm < (2 << 20)
}

fn fits_i26(imm: i32) -> bool {
    -(1i32 << 25) <= imm && imm < (1i32 << 25)
}

#[cfg(test)]
mod tests {
    use super::*;
    use cpu::arm64::reg::*;

    macro_rules! assert_emit {
        (
            $exp:expr;
            $val:expr
        ) => {{
            let exp: u32 = $exp;
            let val: u32 = $val;

            if exp != val {
                panic!("0x{:08X} != 0x{:08X}", exp, val);
            }
        }};
    }

    #[test]
    fn test_fits_bit() {
        assert!(fits_bit(0));
        assert!(fits_bit(1));
        assert!(!fits_bit(2));
    }

    #[test]
    fn test_fits_u4() {
        assert!(fits_u4(0));
        assert!(fits_u4(1));
        assert!(fits_u4(14));
        assert!(fits_u4(15));
        assert!(!fits_u4(16));
        assert!(!fits_u4(17));
    }

    #[test]
    fn test_fits_u5() {
        assert!(fits_u5(0));
        assert!(fits_u5(31));
        assert!(!fits_u5(32));
        assert!(!fits_u5(33));
    }

    #[test]
    fn test_fits_u7() {
        assert!(fits_u7(0));
        assert!(fits_u7(31));
        assert!(fits_u7(126));
        assert!(fits_u7(127));
        assert!(!fits_u7(128));
        assert!(!fits_u7(129));
    }

    #[test]
    fn test_fits_u12() {
        assert!(fits_u12(0));
        assert!(fits_u12(4095));
        assert!(!fits_u12(4096));
        assert!(!fits_u12(4097));
    }

    #[test]
    fn test_fits_i12() {
        assert!(fits_i12(0));
        assert!(fits_i12(-2048));
        assert!(fits_i12(2047));
        assert!(!fits_i12(-2049));
        assert!(!fits_i12(2048));
    }

    #[test]
    fn test_br_blr() {
        assert_emit!(0xd61f0000; br(R0));
        assert_emit!(0xd61f03c0; br(R30));
        assert_emit!(0xd63f0000; blr(R0));
        assert_emit!(0xd63f03c0; blr(R30));
    }

    #[test]
    fn test_nop() {
        assert_emit!(0xd503201f; nop());
    }

    #[test]
    fn test_ret() {
        assert_emit!(0xd65f03c0; ret());
        assert_emit!(0xd65f0000; ret_reg(R0));
        assert_emit!(0xd65f0140; ret_reg(R10));
    }

    #[test]
    fn test_add_imm() {
        assert_emit!(0x11000420; add_imm(0, R0, R1, 1, 0));
        assert_emit!(0x11400c62; add_imm(0, R2, R3, 3, 1));
        assert_emit!(0x91000420; add_imm(1, R0, R1, 1, 0));
        assert_emit!(0x91400c62; add_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_sub_imm() {
        assert_emit!(0x51000420; sub_imm(0, R0, R1, 1, 0));
        assert_emit!(0x51400c62; sub_imm(0, R2, R3, 3, 1));
        assert_emit!(0xd1000420; sub_imm(1, R0, R1, 1, 0));
        assert_emit!(0xd1400c62; sub_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_add_shreg() {
        assert_emit!(0x0b030441; add_shreg(0, R1, R2, R3, Shift::LSL, 1));
        assert_emit!(0x8b0608a4; add_shreg(1, R4, R5, R6, Shift::LSL, 2));
        assert_emit!(0x0b430441; add_shreg(0, R1, R2, R3, Shift::LSR, 1));
        assert_emit!(0x8b8608a4; add_shreg(1, R4, R5, R6, Shift::ASR, 2));
    }

    #[test]
    fn test_sub_shreg() {
        assert_emit!(0x4b030441; sub_shreg(0, R1, R2, R3, Shift::LSL, 1));
        assert_emit!(0xcb0608a4; sub_shreg(1, R4, R5, R6, Shift::LSL, 2));
        assert_emit!(0x4b430441; sub_shreg(0, R1, R2, R3, Shift::LSR, 1));
        assert_emit!(0xcb8608a4; sub_shreg(1, R4, R5, R6, Shift::ASR, 2));
    }

    #[test]
    fn test_add_reg() {
        assert_emit!(0x0b010000; add_reg(0, R0, R0, R1));
        assert_emit!(0x8b010000; add_reg(1, R0, R0, R1));
        assert_emit!(0x0b030041; add_reg(0, R1, R2, R3));
        assert_emit!(0x8b030041; add_reg(1, R1, R2, R3));
    }

    #[test]
    fn test_sub_reg() {
        assert_emit!(0x4b010000; sub_reg(0, R0, R0, R1));
        assert_emit!(0xcb010000; sub_reg(1, R0, R0, R1));
        assert_emit!(0x4b030041; sub_reg(0, R1, R2, R3));
        assert_emit!(0xcb030041; sub_reg(1, R1, R2, R3));
    }

    #[test]
    fn test_ldr_imm() {
        assert_emit!(0x39400420; ldrb_imm(R0, R1, 1));
        assert_emit!(0x39400862; ldrb_imm(R2, R3, 2));

        assert_emit!(0x79400420; ldrh_imm(R0, R1, 1));
        assert_emit!(0x79400862; ldrh_imm(R2, R3, 2));

        assert_emit!(0xb9400420; ldrw_imm(R0, R1, 1));
        assert_emit!(0xb9400862; ldrw_imm(R2, R3, 2));

        assert_emit!(0xf9400420; ldrx_imm(R0, R1, 1));
        assert_emit!(0xf9400862; ldrx_imm(R2, R3, 2));
    }

    #[test]
    fn test_ldr_literal() {
        // forward jump
        assert_emit!(0x18000060; ldrw_literal(R0, 3));
        assert_emit!(0x58000040; ldrx_literal(R0, 2));
        assert_emit!(0x1800007e; ldrw_literal(R30, 3));
        assert_emit!(0x5800005e; ldrx_literal(R30, 2));

        // backward jump
        assert_emit!(0x18ffffe0; ldrw_literal(R0, -1));
        assert_emit!(0x58ffffc0; ldrx_literal(R0, -2));
        assert_emit!(0x18fffffe; ldrw_literal(R30, -1));
        assert_emit!(0x58ffffde; ldrx_literal(R30, -2));
    }

    #[test]
    fn test_ldr_ind() {
        assert_emit!(0x38626820; ldrb_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0x38656883; ldrb_ind(R3, R4, R5, LdStExtend::LSL, 0));

        assert_emit!(0x78626820; ldrh_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0x78656883; ldrh_ind(R3, R4, R5, LdStExtend::LSL, 0));

        assert_emit!(0xb8626820; ldrw_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0xb8657883; ldrw_ind(R3, R4, R5, LdStExtend::LSL, 1));
        assert_emit!(0xb86858e6; ldrw_ind(R6, R7, R8, LdStExtend::UXTW, 1));
        assert_emit!(0xb86bd949; ldrw_ind(R9, R10, R11, LdStExtend::SXTW, 1));

        assert_emit!(0xf8626820; ldrx_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0xf8657883; ldrx_ind(R3, R4, R5, LdStExtend::LSL, 1));
        assert_emit!(0xf86858e6; ldrx_ind(R6, R7, R8, LdStExtend::UXTW, 1));
        assert_emit!(0xf86bd949; ldrx_ind(R9, R10, R11, LdStExtend::SXTW, 1));
    }

    #[test]
    fn test_str_imm() {
        assert_emit!(0x39000420; strb_imm(R0, R1, 1));
        assert_emit!(0x39000862; strb_imm(R2, R3, 2));

        assert_emit!(0x79000420; strh_imm(R0, R1, 1));
        assert_emit!(0x79000862; strh_imm(R2, R3, 2));

        assert_emit!(0xb9000420; strw_imm(R0, R1, 1));
        assert_emit!(0xb9000862; strw_imm(R2, R3, 2));

        assert_emit!(0xf9000420; strx_imm(R0, R1, 1));
        assert_emit!(0xf9000862; strx_imm(R2, R3, 2));
    }

    #[test]
    fn test_str_ind() {
        assert_emit!(0x38226820; strb_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0x38256883; strb_ind(R3, R4, R5, LdStExtend::LSL, 0));

        assert_emit!(0x78226820; strh_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0x78256883; strh_ind(R3, R4, R5, LdStExtend::LSL, 0));

        assert_emit!(0xb8226820; strw_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0xb8257883; strw_ind(R3, R4, R5, LdStExtend::LSL, 1));
        assert_emit!(0xb82858e6; strw_ind(R6, R7, R8, LdStExtend::UXTW, 1));
        assert_emit!(0xb82bd949; strw_ind(R9, R10, R11, LdStExtend::SXTW, 1));

        assert_emit!(0xf8226820; strx_ind(R0, R1, R2, LdStExtend::LSL, 0));
        assert_emit!(0xf8257883; strx_ind(R3, R4, R5, LdStExtend::LSL, 1));
        assert_emit!(0xf82858e6; strx_ind(R6, R7, R8, LdStExtend::UXTW, 1));
        assert_emit!(0xf82bd949; strx_ind(R9, R10, R11, LdStExtend::SXTW, 1));
    }

    #[test]
    fn test_logical_shreg() {
        assert_emit!(0x0a020420; and_shreg(0, R0, R1, R2, Shift::LSL, 1));
        assert_emit!(0x0a650883; bic_shreg(0, R3, R4, R5, Shift::LSR, 2));
        assert_emit!(0xaa880ce6; orr_shreg(1, R6, R7, R8, Shift::ASR, 3));
        assert_emit!(0xaaeb1149; orn_shreg(1, R9, R10, R11, Shift::ROR, 4));
        assert_emit!(0xca0e15ac; eor_shreg(1, R12, R13, R14, Shift::LSL, 5));
        assert_emit!(0xca711a0f; eon_shreg(1, R15, R16, R17, Shift::LSR, 6));
        assert_emit!(0xea941e72; ands_shreg(1, R18, R19, R20, Shift::ASR, 7));
        assert_emit!(0xeaf726d5; bics_shreg(1, R21, R22, R23, Shift::ROR, 9));
    }

    #[test]
    fn test_brk() {
        assert_emit!(0xd4200000; brk(0));
        assert_emit!(0xd43fffe0; brk(0xFFFF));
    }

    #[test]
    fn test_b_imm() {
        assert_emit!(0x14000000; b_imm(0));
        assert_emit!(0x17FFFFFF; b_imm(-1));
        assert_emit!(0x14000001; b_imm(1));
    }

    #[test]
    fn test_bl_imm() {
        assert_emit!(0x94000000; bl_imm(0));
        assert_emit!(0x97FFFFFF; bl_imm(-1));
        assert_emit!(0x94000001; bl_imm(1));
    }

    #[test]
    fn test_b_cond_imm() {
        assert_emit!(0x54ffffe0; b_cond_imm(Cond::EQ, -1));
        assert_emit!(0x54ffffc1; b_cond_imm(Cond::NE, -2));
        assert_emit!(0x54000044; b_cond_imm(Cond::MI,  2));
        assert_emit!(0x5400002b; b_cond_imm(Cond::LT,  1));
    }

    #[test]
    fn test_adds_subs() {
        assert_emit!(0x2b030041; adds_reg(0, R1, R2, R3));
        assert_emit!(0xeb0600a4; subs_reg(1, R4, R5, R6));
        assert_emit!(0x2b830841; adds_shreg(0, R1, R2, R3, Shift::ASR, 2));
        assert_emit!(0xeb060ca4; subs_shreg(1, R4, R5, R6, Shift::LSL, 3));
    }

    #[test]
    fn test_cmp() {
        assert_emit!(0x6b0600bf; cmp_reg(0, R5, R6));
        assert_emit!(0x6b060cbf; cmp_shreg(0, R5, R6, Shift::LSL, 3));
        assert_emit!(0xeb0600bf; cmp_reg(1, R5, R6));
        assert_emit!(0xeb060cbf; cmp_shreg(1, R5, R6, Shift::LSL, 3));
    }

    #[test]
    fn test_csel() {
        assert_emit!(0x1a821020; csel(0, R0, R1, R2, Cond::NE));
        assert_emit!(0x9a856083; csel(1, R3, R4, R5, Cond::VS));
    }

    #[test]
    fn test_csinc() {
        assert_emit!(0x1a821420; csinc(0, R0, R1, R2, Cond::NE));
        assert_emit!(0x9a856483; csinc(1, R3, R4, R5, Cond::VS));
    }

    #[test]
    fn test_cset() {
        assert_emit!(0x1a9f17e0; cset(0, R0, Cond::EQ));
        assert_emit!(0x9a9fc7e3; cset(1, R3, Cond::LE));

        assert_emit!(0x1a9f17e0; cset(0, R0, Cond::EQ));
        assert_emit!(0x1a9f07e0; cset(0, R0, Cond::NE));
        assert_emit!(0x1a9f37e0; cset(0, R0, Cond::CS));
        assert_emit!(0x1a9f37e0; cset(0, R0, Cond::HS));

        assert_emit!(0x1a9f27e0; cset(0, R0, Cond::CC));
        assert_emit!(0x1a9f27e0; cset(0, R0, Cond::LO));
        assert_emit!(0x1a9f57e0; cset(0, R0, Cond::MI));
        assert_emit!(0x1a9f47e0; cset(0, R0, Cond::PL));

        assert_emit!(0x1a9f77e0; cset(0, R0, Cond::VS));
        assert_emit!(0x1a9f67e0; cset(0, R0, Cond::VC));
        assert_emit!(0x1a9f97e0; cset(0, R0, Cond::HI));
        assert_emit!(0x1a9f87e0; cset(0, R0, Cond::LS));

        assert_emit!(0x1a9fb7e0; cset(0, R0, Cond::GE));
        assert_emit!(0x1a9fa7e0; cset(0, R0, Cond::LT));
        assert_emit!(0x1a9fd7e0; cset(0, R0, Cond::GT));
        assert_emit!(0x1a9fc7e0; cset(0, R0, Cond::LE));
    }

    #[test]
    fn test_mov_imm() {
        assert_emit!(0x12800100; movn(0, R0, 8, 0));
        assert_emit!(0x52800100; movz(0, R0, 8, 0));
        assert_emit!(0x72a00100; movk(0, R0, 8, 1));
    }

    #[test]
    fn test_adr_adrp() {
        assert_emit!(0x10ffffe0; adr(R0 , -4));
        assert_emit!(0x10ffffde; adr(R30, -8));
        assert_emit!(0x1000001d; adr(R29,  0));
        assert_emit!(0x1000003c; adr(R28,  4));
        assert_emit!(0x1000005b; adr(R27,  8));

        assert_emit!(0x90ffffe0; adrp(R0 , -4));
        assert_emit!(0x90ffffde; adrp(R30, -8));
        assert_emit!(0x9000001d; adrp(R29,  0));
        assert_emit!(0x9000003c; adrp(R28,  4));
        assert_emit!(0x9000005b; adrp(R27,  8));
    }

    #[test]
    fn test_div() {
        assert_emit!(0x1ac20820; udiv(0, R0, R1, R2));
        assert_emit!(0x9ac50c83; sdiv(1, R3, R4, R5));
        assert_emit!(0x1ac820e6; lslv(0, R6, R7, R8));
        assert_emit!(0x1acb2549; lsrv(0, R9, R10, R11));
        assert_emit!(0x1ace29ac; asrv(0, R12, R13, R14));
        assert_emit!(0x1ad12e0f; rorv(0, R15, R16, R17));
    }

    #[test]
    fn test_ldp() {
        assert_emit!(0x29400440; ldpw(R0, R1, R2, 0));
        assert_emit!(0x294090a3; ldpw(R3, R4, R5, 1));
        assert_emit!(0x294110a3; ldpw(R3, R4, R5, 2));
        assert_emit!(0xa9400440; ldpx(R0, R1, R2, 0));
        assert_emit!(0xa94090a3; ldpx(R3, R4, R5, 1));
        assert_emit!(0xa94110a3; ldpx(R3, R4, R5, 2));
    }

    #[test]
    fn test_stp() {
        assert_emit!(0x29000440; stpw(R0, R1, R2, 0));
        assert_emit!(0x290090a3; stpw(R3, R4, R5, 1));
        assert_emit!(0x290110a3; stpw(R3, R4, R5, 2));
        assert_emit!(0xa9000440; stpx(R0, R1, R2, 0));
        assert_emit!(0xa90090a3; stpx(R3, R4, R5, 1));
        assert_emit!(0xa90110a3; stpx(R3, R4, R5, 2));
    }

    #[test]
    fn test_madd_msub() {
        assert_emit!(0x9b031041; madd(1, R1, R2, R3, R4));
        assert_emit!(0x1b0720c5; madd(0, R5, R6, R7, R8));
        assert_emit!(0x9b0bb149; msub(1, R9, R10, R11, R12));
        assert_emit!(0x1b0fc1cd; msub(0, R13, R14, R15, R16));
    }

    #[test]
    fn test_mul() {
        assert_emit!(0x9b037c41; mul(1, R1, R2, R3));
        assert_emit!(0x1b067ca4; mul(0, R4, R5, R6));
    }
}