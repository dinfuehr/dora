use crate::cpu::arm64::reg::*;
use crate::cpu::{FReg, Reg};
use crate::masm::CondCode;

pub fn add_extreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, extend: Extend, amount: u32) -> u32 {
    cls_addsub_extreg(sf, 0, 0, 0, rm, extend, amount, rn, rd)
}

pub fn sub_extreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, extend: Extend, amount: u32) -> u32 {
    cls_addsub_extreg(sf, 1, 0, 0, rm, extend, amount, rn, rd)
}

fn cls_addsub_extreg(
    sf: u32,
    op: u32,
    s: u32,
    opt: u32,
    rm: Reg,
    option: Extend,
    imm3: u32,
    rn: Reg,
    rd: Reg,
) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(opt == 0);
    assert!(rm.is_gpr_or_zero());
    assert!(fits_u2(imm3));
    assert!(rn.is_gpr_or_sp());
    assert!(rd.is_gpr_or_sp());

    sf << 31
        | op << 30
        | s << 29
        | 0b01011u32 << 24
        | opt << 22
        | 1u32 << 21
        | rm.asm() << 16
        | option.u32() << 13
        | imm3 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn stp_pre(sf: u32, rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    assert!(fits_bit(sf));

    let opc = if sf != 0 { 0b10 } else { 0b00 };
    cls_ldst_pair_pre(opc, 0, 0, imm7, rt2, rn, rt)
}

fn cls_ldst_pair_pre(opc: u32, v: u32, l: u32, imm7: i32, rt2: Reg, rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(opc));
    assert!(fits_bit(v));
    assert!(fits_bit(l));
    assert!(fits_i7(imm7));
    assert!(rt2.is_gpr());
    assert!(rn.is_gpr_or_sp());
    assert!(rt.is_gpr());

    let imm7 = (imm7 as u32) & 0x7F;

    opc << 30
        | 0b101u32 << 27
        | v << 26
        | 0b011u32 << 23
        | l << 22
        | imm7 << 15
        | rt2.asm() << 10
        | rn.asm() << 5
        | rt.asm()
}

pub fn ldp_post(sf: u32, rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    assert!(fits_bit(sf));

    let opc = if sf != 0 { 0b10 } else { 0b00 };
    cls_ldst_pair_post(opc, 0, 1, imm7, rt2, rn, rt)
}

fn cls_ldst_pair_post(opc: u32, v: u32, l: u32, imm7: i32, rt2: Reg, rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(opc));
    assert!(fits_bit(v));
    assert!(fits_bit(l));
    assert!(fits_i7(imm7));
    assert!(rt2.is_gpr());
    assert!(rn.is_gpr_or_sp());
    assert!(rt.is_gpr());

    let imm7 = (imm7 as u32) & 0x7F;

    opc << 30
        | 0b101u32 << 27
        | v << 26
        | 0b001u32 << 23
        | l << 22
        | imm7 << 15
        | rt2.asm() << 10
        | rn.asm() << 5
        | rt.asm()
}

pub fn b_cond_imm(cond: Cond, imm19: i32) -> u32 {
    cls_cond_branch_imm(cond, imm19)
}

fn cls_cond_branch_imm(cond: Cond, imm19: i32) -> u32 {
    assert!(fits_i19(imm19));

    let imm = (imm19 as u32) & 0x7FFFF;

    0b01010100u32 << 24 | imm << 5 | cond.u32()
}

pub fn add_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 0, 0, shift, imm12, rn, rd)
}

pub fn adds_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 0, 1, shift, imm12, rn, rd)
}

pub fn sub_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 1, 0, shift, imm12, rn, rd)
}

pub fn subs_imm(sf: u32, rd: Reg, rn: Reg, imm12: u32, shift: u32) -> u32 {
    cls_addsub_imm(sf, 1, 1, shift, imm12, rn, rd)
}

pub fn cmp_imm(sf: u32, rn: Reg, imm12: u32, shift: u32) -> u32 {
    subs_imm(sf, REG_ZERO, rn, imm12, shift)
}

fn cls_addsub_imm(sf: u32, op: u32, s: u32, shift: u32, imm12: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(fits_bit(shift));
    assert!(fits_u12(imm12));
    assert!(rn.is_gpr_or_sp());

    if s != 0 {
        assert!(rd.is_gpr_or_zero());
    } else {
        assert!(rd.is_gpr_or_sp());
    }

    (0b10001 as u32) << 24
        | sf << 31
        | op << 30
        | s << 29
        | shift << 22
        | imm12 << 10
        | rn.asm() << 5
        | rd.asm()
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

fn cls_addsub_shreg(
    sf: u32,
    op: u32,
    s: u32,
    shift: Shift,
    rm: Reg,
    imm6: u32,
    rn: Reg,
    rd: Reg,
) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(!shift.is_ror());
    assert!(rm.is_gpr());
    assert!(fits_u5(imm6));
    assert!(rn.is_gpr_or_zero());
    assert!(rd.is_gpr_or_zero());

    0b01011u32 << 24
        | sf << 31
        | op << 30
        | s << 29
        | shift.u32() << 22
        | rm.asm() << 16
        | imm6 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn ldrb_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regoffset(0b00, 0, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn ldrh_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regoffset(0b01, 0, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn ldrw_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regoffset(0b10, 0, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn ldrx_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regoffset(0b11, 0, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn ldrs_ind(rt: FReg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b10, 1, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn ldrd_ind(rt: FReg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b11, 1, 0b01, rm, extend, amount, rn, rt.asm())
}

pub fn strb_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regoffset(0b00, 0, 0b00, rm, extend, amount, rn, rt.asm())
}

pub fn strh_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regoffset(0b01, 0, 0b00, rm, extend, amount, rn, rt.asm())
}

pub fn strw_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regoffset(0b10, 0, 0b00, rm, extend, amount, rn, rt.asm())
}

pub fn strx_ind(rt: Reg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regoffset(0b11, 0, 0b00, rm, extend, amount, rn, rt.asm())
}

pub fn strs_ind(rt: FReg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b10, 1, 0b00, rm, extend, amount, rn, rt.asm())
}

pub fn strd_ind(rt: FReg, rn: Reg, rm: Reg, extend: LdStExtend, amount: u32) -> u32 {
    cls_ldst_regoffset(0b11, 1, 0b00, rm, extend, amount, rn, rt.asm())
}

fn cls_ldst_regoffset(
    size: u32,
    v: u32,
    opc: u32,
    rm: Reg,
    option: LdStExtend,
    s: u32,
    rn: Reg,
    rt: u32,
) -> u32 {
    assert!(fits_u2(size));
    assert!(fits_bit(v));
    assert!(fits_u2(opc));
    assert!(rm.is_gpr_or_zero());
    assert!(fits_bit(s));
    assert!(rn.is_gpr_or_sp());
    assert!(fits_u5(rt));

    0b111u32 << 27
        | 1u32 << 21
        | 0b10u32 << 10
        | size << 30
        | v << 26
        | opc << 22
        | rm.asm() << 16
        | option.u32() << 13
        | s << 12
        | rn.asm() << 5
        | rt
}

pub fn ldrb_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regimm(0b00, 0, 0b01, imm12, rn, rt.asm())
}

pub fn ldrh_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regimm(0b01, 0, 0b01, imm12, rn, rt.asm())
}

pub fn ldrw_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regimm(0b10, 0, 0b01, imm12, rn, rt.asm())
}

pub fn ldrx_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_regimm(0b11, 0, 0b01, imm12, rn, rt.asm())
}

pub fn ldrs_imm(rt: FReg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b10, 1, 0b01, imm12, rn, rt.asm())
}

pub fn ldrd_imm(rt: FReg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b11, 1, 0b01, imm12, rn, rt.asm())
}

pub fn strb_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regimm(0b00, 0, 0b00, imm12, rn, rt.asm())
}

pub fn strh_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regimm(0b01, 0, 0b00, imm12, rn, rt.asm())
}

pub fn strw_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regimm(0b10, 0, 0b00, imm12, rn, rt.asm())
}

pub fn strx_imm(rt: Reg, rn: Reg, imm12: u32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    cls_ldst_regimm(0b11, 0, 0b00, imm12, rn, rt.asm())
}

pub fn strs_imm(rt: FReg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b10, 1, 0b00, imm12, rn, rt.asm())
}

pub fn strd_imm(rt: FReg, rn: Reg, imm12: u32) -> u32 {
    cls_ldst_regimm(0b11, 1, 0b00, imm12, rn, rt.asm())
}

fn cls_ldst_regimm(size: u32, v: u32, opc: u32, imm12: u32, rn: Reg, rt: u32) -> u32 {
    assert!(fits_u2(size));
    assert!(fits_bit(v));
    assert!(fits_u2(opc));
    assert!(fits_u12(imm12));
    assert!(rn.is_gpr_or_sp());
    assert!(fits_u5(rt));

    0b111001u32 << 24 | size << 30 | v << 26 | opc << 22 | imm12 << 10 | rn.asm() << 5 | rt
}

pub fn ldrb_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_reg_unscaledimm(0b00, 0, 0b01, imm9, rn, rt.asm())
}

pub fn ldrh_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_reg_unscaledimm(0b01, 0, 0b01, imm9, rn, rt.asm())
}

pub fn ldrw_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_reg_unscaledimm(0b10, 0, 0b01, imm9, rn, rt.asm())
}

pub fn ldrx_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr());
    cls_ldst_reg_unscaledimm(0b11, 0, 0b01, imm9, rn, rt.asm())
}

pub fn ldrs_unscaled_imm(rt: FReg, rn: Reg, imm9: i32) -> u32 {
    cls_ldst_reg_unscaledimm(0b10, 1, 0b01, imm9, rn, rt.asm())
}

pub fn ldrd_unscaled_imm(rt: FReg, rn: Reg, imm9: i32) -> u32 {
    cls_ldst_reg_unscaledimm(0b11, 1, 0b01, imm9, rn, rt.asm())
}

pub fn strb_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    assert!(rn.is_gpr_or_sp());
    cls_ldst_reg_unscaledimm(0b00, 0, 0b00, imm9, rn, rt.asm())
}

pub fn strh_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    assert!(rn.is_gpr_or_sp());
    cls_ldst_reg_unscaledimm(0b01, 0, 0b00, imm9, rn, rt.asm())
}

pub fn strw_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    assert!(rn.is_gpr_or_sp());
    cls_ldst_reg_unscaledimm(0b10, 0, 0b00, imm9, rn, rt.asm())
}

pub fn strx_unscaled_imm(rt: Reg, rn: Reg, imm9: i32) -> u32 {
    assert!(rt.is_gpr_or_zero());
    assert!(rn.is_gpr_or_sp());
    cls_ldst_reg_unscaledimm(0b11, 0, 0b00, imm9, rn, rt.asm())
}

pub fn strs_unscaled_imm(rt: FReg, rn: Reg, imm9: i32) -> u32 {
    cls_ldst_reg_unscaledimm(0b10, 1, 0b00, imm9, rn, rt.asm())
}

pub fn strd_unscaled_imm(rt: FReg, rn: Reg, imm9: i32) -> u32 {
    cls_ldst_reg_unscaledimm(0b11, 1, 0b00, imm9, rn, rt.asm())
}

fn cls_ldst_reg_unscaledimm(size: u32, v: u32, opc: u32, imm9: i32, rn: Reg, rt: u32) -> u32 {
    assert!(fits_u2(size));
    assert!(fits_bit(v));
    assert!(fits_u2(opc));
    assert!(fits_i9(imm9));
    assert!(rn.is_gpr_or_sp());
    assert!(fits_u5(rt));

    let imm = (imm9 as u32) & 0x1FF;

    size << 30 | 0b111 << 27 | v << 26 | opc << 22 | imm << 12 | rn.asm() << 5 | rt
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

    0b011u32 << 27 | opc << 30 | v << 26 | imm << 5 | rt.asm()
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

fn cls_logical_shreg(
    sf: u32,
    opc: u32,
    shift: Shift,
    n: u32,
    rm: Reg,
    imm6: u32,
    rn: Reg,
    rd: Reg,
) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_bit(n));
    assert!(rm.is_gpr_or_zero());
    assert!(fits_u5(imm6));
    assert!(rn.is_gpr_or_zero());
    assert!(rd.is_gpr());

    0b01010u32 << 24
        | sf << 31
        | opc << 29
        | shift.u32() << 22
        | n << 21
        | rm.asm() << 16
        | imm6 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn csel(sf: u32, rd: Reg, rn: Reg, rm: Reg, cond: Cond) -> u32 {
    cls_csel(sf, 0, 0, rm, cond, 0, rn, rd)
}

pub fn csinc(sf: u32, rd: Reg, rn: Reg, rm: Reg, cond: Cond) -> u32 {
    cls_csel(sf, 0, 0, rm, cond, 1, rn, rd)
}

pub fn csinv(sf: u32, rd: Reg, rn: Reg, rm: Reg, cond: Cond) -> u32 {
    cls_csel(sf, 1, 0, rm, cond, 0, rn, rd)
}

pub fn cset(sf: u32, rd: Reg, cond: Cond) -> u32 {
    csinc(sf, rd, REG_ZERO, REG_ZERO, cond.invert())
}

fn cls_csel(sf: u32, op: u32, s: u32, rm: Reg, cond: Cond, op2: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(rm.is_gpr_or_zero());
    assert!(fits_bit(op2));
    assert!(rn.is_gpr_or_zero());
    assert!(rd.is_gpr());

    0b11010100u32 << 21
        | sf << 31
        | op << 30
        | s << 29
        | rm.asm() << 16
        | cond.u32() << 12
        | op2 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn movn(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b00, shift, imm16, rd)
}

pub fn movz(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b10, shift, imm16, rd)
}

pub fn movk(sf: u32, rd: Reg, imm16: u32, shift: u32) -> u32 {
    cls_move_wide_imm(sf, 0b11, shift, imm16, rd)
}

fn cls_move_wide_imm(sf: u32, opc: u32, hw: u32, imm16: u32, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_u2(hw));
    if sf == 0 {
        assert!(fits_bit(hw));
    }
    assert!(fits_u16(imm16));
    assert!(rd.is_gpr());

    0b100101u32 << 23 | sf << 31 | opc << 29 | hw << 21 | imm16 << 5 | rd.asm()
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
    assert!(rd.is_gpr());

    let imm = imm as u32;

    let immlo = imm & 3;
    let immhi = (imm >> 2) & 0x7FFFF;

    1u32 << 28 | op << 31 | immlo << 29 | immhi << 5 | rd.asm()
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

fn cls_dataproc3(
    sf: u32,
    op54: u32,
    op31: u32,
    rm: Reg,
    o0: u32,
    ra: Reg,
    rn: Reg,
    rd: Reg,
) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(op54));
    assert!(fits_u3(op31));
    assert!(rm.is_gpr());
    assert!(fits_bit(o0));
    assert!(ra.is_gpr_or_zero());
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31
        | op54 << 29
        | 0b11011u32 << 24
        | op31 << 21
        | rm.asm() << 16
        | o0 << 15
        | ra.asm() << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn ldp(sf: u32, rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    assert!(fits_bit(sf));
    let opc = if sf != 0 { 0b10 } else { 0b00 };

    cls_ldst_pair(opc, 0, 1, imm7, rt2, rn, rt)
}

pub fn stp(sf: u32, rt: Reg, rt2: Reg, rn: Reg, imm7: i32) -> u32 {
    assert!(fits_bit(sf));
    let opc = if sf != 0 { 0b10 } else { 0b00 };

    cls_ldst_pair(opc, 0, 0, imm7, rt2, rn, rt)
}

fn cls_ldst_pair(opc: u32, v: u32, l: u32, imm7: i32, rt2: Reg, rn: Reg, rt: Reg) -> u32 {
    assert!(fits_u2(opc));
    assert!(fits_bit(v));
    assert!(fits_bit(l));
    assert!(fits_i7(imm7));
    assert!(rt2.is_gpr());
    assert!(rn.is_gpr_or_sp());
    assert!(rt.is_gpr());

    let imm = (imm7 as u32) & 0x7F;

    opc << 30
        | 0b101u32 << 27
        | 1u32 << 24
        | l << 22
        | imm << 15
        | rt2.asm() << 10
        | rn.asm() << 5
        | rt.asm()
}

pub fn sbfm(sf: u32, rd: Reg, rn: Reg, immr: u32, imms: u32) -> u32 {
    cls_bitfield(sf, 0b00, sf, immr, imms, rn, rd)
}

pub fn bfm(sf: u32, rd: Reg, rn: Reg, immr: u32, imms: u32) -> u32 {
    cls_bitfield(sf, 0b01, sf, immr, imms, rn, rd)
}

pub fn ubfm(sf: u32, rd: Reg, rn: Reg, immr: u32, imms: u32) -> u32 {
    cls_bitfield(sf, 0b10, sf, immr, imms, rn, rd)
}

pub fn lsr_imm(sf: u32, rd: Reg, rn: Reg, shift: u32) -> u32 {
    let val = if sf != 0 { 64 } else { 32 };
    ubfm(sf, rd, rn, shift, val - 1)
}

pub fn lsl_imm(sf: u32, rd: Reg, rn: Reg, shift: u32) -> u32 {
    let (val, mask) = if sf != 0 { (64, 0x3f) } else { (32, 0x1f) };
    ubfm(sf, rd, rn, (val - shift) & mask, val - 1 - shift)
}

pub fn uxtb(rd: Reg, rn: Reg) -> u32 {
    ubfm(0, rd, rn, 0, 7)
}

pub fn sxtw(rd: Reg, rn: Reg) -> u32 {
    sbfm(1, rd, rn, 0, 31)
}

pub fn uxtw(rd: Reg, rn: Reg) -> u32 {
    ubfm(1, rd, rn, 0, 31)
}

fn cls_bitfield(sf: u32, opc: u32, n: u32, immr: u32, imms: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_bit(n));
    assert!(fits_u6(immr));
    assert!(fits_u6(imms));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31
        | opc << 29
        | 0b100110u32 << 23
        | n << 22
        | (immr & 0x3F) << 16
        | (imms & 0x3F) << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn and_imm(sf: u32, rd: Reg, rn: Reg, imm: u64) -> u32 {
    let reg_size = if sf == 1 { 64 } else { 32 };
    let n_immr_imms = encode_logical_imm(imm, reg_size).unwrap();
    cls_logical_imm(sf, 0b00, n_immr_imms, rn, rd)
}

fn encode_logical_imm(mut imm: u64, reg_size: u32) -> Option<u32> {
    assert!(reg_size == 32 || reg_size == 64);

    if imm == 0 || imm == !0 || (reg_size != 64 && (imm >> reg_size != 0 || imm == !0u32 as u64)) {
        return None;
    }

    // determine element size, use smallest possible element size
    let mut size: u32 = reg_size;

    while size > 2 {
        size /= 2;

        let mask = (1 << size) - 1;

        if (imm & mask) != (imm >> size) & mask {
            size *= 2;
            break;
        }
    }

    // truncate immediate to element size
    let mask = !0u64 >> (64 - size);
    imm &= mask;

    let rotation;
    let ones;

    if is_shifted_mask(imm) {
        let tz = imm.trailing_zeros();

        rotation = (size - tz) & (size - 1);
        ones = (!(imm >> tz)).trailing_zeros();

    // not all immediates are shifted masks: e.g. 1001
    } else {
        // extend imm again to 64 bits: e.g. 1..1|1001
        imm |= !mask;

        // the negation of the immediate now needs to be
        // a shifted mask
        if !is_shifted_mask(!imm) {
            return None;
        }

        let lo = (!imm).leading_zeros();
        let to = (!imm).trailing_zeros();

        rotation = lo - (64 - size);
        ones = lo + to - (64 - size);
    }

    assert!(ones > 0);
    assert!(rotation < size);

    let immr = rotation;
    let mut nimms = !(size - 1) << 1;
    nimms |= ones - 1;

    let n = ((nimms >> 6) & 1) ^ 1;

    Some(n << 12 | immr << 6 | nimms & 0x3f)
}

fn is_shifted_mask(imm: u64) -> bool {
    imm != 0 && is_mask((imm - 1) | imm)
}

fn is_mask(imm: u64) -> bool {
    imm != 0 && imm.wrapping_add(1) & imm == 0
}

fn cls_logical_imm(sf: u32, opc: u32, n_immr_imms: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_u2(opc));
    assert!(fits_u13(n_immr_imms));
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());

    sf << 31 | opc << 29 | 0b100100u32 << 23 | n_immr_imms << 10 | rn.asm() << 5 | rd.asm()
}

fn cls_fp_dataproc2(m: u32, s: u32, ty: u32, rm: FReg, opcode: u32, rn: FReg, rd: FReg) -> u32 {
    assert!(m == 0);
    assert!(s == 0);
    assert!(fits_bit(ty));
    assert!(fits_u4(opcode));

    m << 31
        | s << 29
        | 0b11110 << 24
        | ty << 22
        | 1 << 21
        | rm.asm() << 16
        | opcode << 12
        | 0b10 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn fadd(ty: u32, rd: FReg, rn: FReg, rm: FReg) -> u32 {
    cls_fp_dataproc2(0, 0, ty, rm, 0b0010, rn, rd)
}

pub fn fsub(ty: u32, rd: FReg, rn: FReg, rm: FReg) -> u32 {
    cls_fp_dataproc2(0, 0, ty, rm, 0b0011, rn, rd)
}

pub fn fmul(ty: u32, rd: FReg, rn: FReg, rm: FReg) -> u32 {
    cls_fp_dataproc2(0, 0, ty, rm, 0b0000, rn, rd)
}

pub fn fdiv(ty: u32, rd: FReg, rn: FReg, rm: FReg) -> u32 {
    cls_fp_dataproc2(0, 0, ty, rm, 0b0001, rn, rd)
}

fn cls_fp_dataproc1(m: u32, s: u32, ty: u32, opcode: u32, rn: FReg, rd: FReg) -> u32 {
    assert!(m == 0);
    assert!(s == 0);
    assert!(fits_u2(ty));
    assert!(fits_u6(opcode));

    m << 31
        | s << 29
        | 0b11110 << 24
        | ty << 22
        | 1 << 21
        | opcode << 15
        | 0b10000 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn fsqrt(ty: u32, rd: FReg, rn: FReg) -> u32 {
    cls_fp_dataproc1(0, 0, ty, 0b000011, rn, rd)
}

pub fn fcvt_sd(rd: FReg, rn: FReg) -> u32 {
    cls_fp_dataproc1(0, 0, 0b00, 0b000101, rn, rd)
}

pub fn fcvt_ds(rd: FReg, rn: FReg) -> u32 {
    cls_fp_dataproc1(0, 0, 0b01, 0b000100, rn, rd)
}

pub fn fmov(ty: u32, rd: FReg, rn: FReg) -> u32 {
    cls_fp_dataproc1(0, 0, ty, 0b000000, rn, rd)
}

pub fn fneg(ty: u32, rd: FReg, rn: FReg) -> u32 {
    cls_fp_dataproc1(0, 0, ty, 0b000010, rn, rd)
}

pub fn scvtf(sf: u32, ty: u32, rd: FReg, rn: Reg) -> u32 {
    cls_fp_int(sf, 0, ty, 0b00, 0b010, rn.asm(), rd.asm())
}

pub fn fcvtzs(sf: u32, ty: u32, rd: Reg, rn: FReg) -> u32 {
    cls_fp_int(sf, 0, ty, 0b11, 0b000, rn.asm(), rd.asm())
}

pub fn fmov_fs(sf: u32, ty: u32, rd: FReg, rn: Reg) -> u32 {
    cls_fp_int(sf, 0, ty, 0b00, 0b111, rn.asm(), rd.asm())
}

pub fn fmov_sf(sf: u32, ty: u32, rd: Reg, rn: FReg) -> u32 {
    cls_fp_int(sf, 0, ty, 0b00, 0b110, rn.asm(), rd.asm())
}

fn cls_fp_int(sf: u32, s: u32, ty: u32, rmode: u32, opcode: u32, rn: u32, rd: u32) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(s));
    assert!(fits_u2(ty));
    assert!(fits_u2(rmode));
    assert!(fits_u3(opcode));
    assert!(fits_u5(rn));
    assert!(fits_u5(rd));

    sf << 31
        | s << 29
        | 0b11110 << 24
        | ty << 22
        | 1 << 21
        | rmode << 19
        | opcode << 16
        | rn << 5
        | rd
}

pub fn cnt(q: u32, size: u32, rd: FReg, rn: FReg) -> u32 {
    cls_simd_2regs_misc(q, 0, size, 0b00101, rn, rd)
}

fn cls_simd_2regs_misc(q: u32, u: u32, size: u32, opcode: u32, rn: FReg, rd: FReg) -> u32 {
    assert!(fits_bit(q));
    assert!(fits_bit(u));
    assert!(fits_u2(size));
    assert!(fits_u5(opcode));

    q << 30
        | u << 29
        | 0b01110 << 24
        | size << 22
        | 0b10000 << 17
        | opcode << 12
        | 0b10 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn addv(q: u32, size: u32, rd: FReg, rn: FReg) -> u32 {
    cls_simd_across_lanes(q, 0, size, 0b11011, rn, rd)
}

fn cls_simd_across_lanes(q: u32, u: u32, size: u32, opcode: u32, rn: FReg, rd: FReg) -> u32 {
    assert!(fits_bit(q));
    assert!(fits_bit(u));
    assert!(fits_u2(size));
    assert!(fits_u5(opcode));

    q << 30
        | u << 29
        | 0b01110 << 24
        | size << 22
        | 0b11000 << 17
        | opcode << 12
        | 0b10 << 10
        | rn.asm() << 5
        | rd.asm()
}

pub fn fcmp(ty: u32, rn: FReg, rm: FReg) -> u32 {
    cls_fp_compare(0, 0, ty, rm, 0, rn, 0)
}

pub fn fcmpe(ty: u32, rn: FReg, rm: FReg) -> u32 {
    cls_fp_compare(0, 0, ty, rm, 0, rn, 0b10000)
}

fn cls_fp_compare(m: u32, s: u32, ty: u32, rm: FReg, op: u32, rn: FReg, opcode2: u32) -> u32 {
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
        | rm.asm() << 16
        | op << 14
        | 0b1000 << 10
        | rn.asm() << 5
        | opcode2
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

impl From<CondCode> for Cond {
    fn from(c: CondCode) -> Cond {
        match c {
            CondCode::Zero => Cond::EQ,
            CondCode::NonZero => Cond::NE,
            CondCode::Equal => Cond::EQ,
            CondCode::NotEqual => Cond::NE,
            CondCode::Greater => Cond::GT,
            CondCode::GreaterEq => Cond::GE,
            CondCode::Less => Cond::LT,
            CondCode::LessEq => Cond::LE,
            CondCode::UnsignedGreater => Cond::HI,
            CondCode::UnsignedGreaterEq => Cond::HS,
            CondCode::UnsignedLess => Cond::LO,
            CondCode::UnsignedLessEq => Cond::LS,
        }
    }
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
    UXTB,
    UXTH,
    LSL,
    UXTW,
    UXTX,
    SXTB,
    SXTH,
    SXTW,
    SXTX,
}

impl Extend {
    fn is_ldr(self) -> bool {
        match self {
            Extend::UXTW | Extend::LSL | Extend::SXTW | Extend::SXTX => true,

            _ => false,
        }
    }

    fn u32(self) -> u32 {
        match self {
            Extend::UXTB => 0b000,
            Extend::UXTH => 0b001,
            Extend::LSL => 0b010,
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
    UXTW,
    LSL,
    SXTW,
    SXTX,
}

impl LdStExtend {
    fn u32(self) -> u32 {
        match self {
            LdStExtend::UXTW => 0b010,
            LdStExtend::LSL => 0b011,
            LdStExtend::SXTW => 0b110,
            LdStExtend::SXTX => 0b111,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Shift {
    LSL,
    LSR,
    ASR,
    ROR,
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

fn fits_u13(imm: u32) -> bool {
    imm < 8192
}

fn fits_u8(imm: u32) -> bool {
    imm < 256
}

fn fits_i7(imm: i32) -> bool {
    -64 <= imm && imm < 64
}

pub fn fits_u12(imm: u32) -> bool {
    imm < 4096
}

pub fn fits_i9(imm: i32) -> bool {
    -256 <= imm && imm < 256
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

pub fn fits_movz(imm: u64, register_size: u32) -> bool {
    assert!(register_size == 32 || register_size == 64);

    count_empty_half_words(imm, register_size) >= (register_size / 16 - 1)
}

pub fn fits_movn(imm: u64, register_size: u32) -> bool {
    fits_movz(!imm, register_size)
}

pub fn shift_movz(mut imm: u64) -> u32 {
    for count in 0..4 {
        if (imm & 0xFFFF) != 0 {
            return count;
        }

        imm >>= 16;
    }

    0
}

pub fn shift_movn(imm: u64) -> u32 {
    shift_movz(!imm)
}

pub fn count_empty_half_words(mut imm: u64, register_size: u32) -> u32 {
    assert!(register_size == 32 || register_size == 64);

    let mut count = 0;

    for _ in 0..(register_size / 16) {
        if (imm & 0xFFFF) == 0 {
            count += 1;
        }

        imm >>= 16;
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_emit {
        ($exp: expr; $val: expr) => {{
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
    fn test_add_imm() {
        assert_emit!(0x11000420; add_imm(0, R0, R1, 1, 0));
        assert_emit!(0x11400c62; add_imm(0, R2, R3, 3, 1));
        assert_emit!(0x91000420; add_imm(1, R0, R1, 1, 0));
        assert_emit!(0x91400c62; add_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_adds_imm() {
        assert_emit!(0x31000420; adds_imm(0, R0, R1, 1, 0));
        assert_emit!(0x31400c62; adds_imm(0, R2, R3, 3, 1));
        assert_emit!(0xb1000420; adds_imm(1, R0, R1, 1, 0));
        assert_emit!(0xb1400c62; adds_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_sub_imm() {
        assert_emit!(0x51000420; sub_imm(0, R0, R1, 1, 0));
        assert_emit!(0x51400c62; sub_imm(0, R2, R3, 3, 1));
        assert_emit!(0xd1000420; sub_imm(1, R0, R1, 1, 0));
        assert_emit!(0xd1400c62; sub_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_subs_imm() {
        assert_emit!(0x71000420; subs_imm(0, R0, R1, 1, 0));
        assert_emit!(0x71400c62; subs_imm(0, R2, R3, 3, 1));
        assert_emit!(0xf1000420; subs_imm(1, R0, R1, 1, 0));
        assert_emit!(0xf1400c62; subs_imm(1, R2, R3, 3, 1));
    }

    #[test]
    fn test_cmp_imm() {
        assert_emit!(0x7100043f; cmp_imm(0, R1, 1, 0));
        assert_emit!(0x71400c5f; cmp_imm(0, R2, 3, 1));
        assert_emit!(0xf100047f; cmp_imm(1, R3, 1, 0));
        assert_emit!(0xf1400c9f; cmp_imm(1, R4, 3, 1));
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
        assert_emit!(0x10000000; adr(R0,  0));
        assert_emit!(0x10000001; adr(R1,  0));

        assert_emit!(0x90ffffe0; adrp(R0 , -4));
        assert_emit!(0x90ffffde; adrp(R30, -8));
        assert_emit!(0x9000001d; adrp(R29,  0));
        assert_emit!(0x9000003c; adrp(R28,  4));
        assert_emit!(0x9000005b; adrp(R27,  8));
        assert_emit!(0x90000000; adrp(R0,  0));
        assert_emit!(0x90000001; adrp(R1,  0));
    }

    #[test]
    fn test_ldp() {
        assert_emit!(0x29400440; ldp(0, R0, R1, R2, 0));
        assert_emit!(0x294090a3; ldp(0, R3, R4, R5, 1));
        assert_emit!(0x294110a3; ldp(0, R3, R4, R5, 2));
        assert_emit!(0xa9400440; ldp(1, R0, R1, R2, 0));
        assert_emit!(0xa94090a3; ldp(1, R3, R4, R5, 1));
        assert_emit!(0xa94110a3; ldp(1, R3, R4, R5, 2));
    }

    #[test]
    fn test_stp() {
        assert_emit!(0x29000440; stp(0, R0, R1, R2, 0));
        assert_emit!(0x290090a3; stp(0, R3, R4, R5, 1));
        assert_emit!(0x290110a3; stp(0, R3, R4, R5, 2));
        assert_emit!(0xa9000440; stp(1, R0, R1, R2, 0));
        assert_emit!(0xa90090a3; stp(1, R3, R4, R5, 1));
        assert_emit!(0xa90110a3; stp(1, R3, R4, R5, 2));
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

    #[test]
    fn test_bfm() {
        assert_emit!(0x53010820; ubfm(0, R0, R1, 1, 2));
        assert_emit!(0xd3431062; ubfm(1, R2, R3, 3, 4));
        assert_emit!(0x33010820; bfm(0, R0, R1, 1, 2));
        assert_emit!(0xb3431062; bfm(1, R2, R3, 3, 4));
        assert_emit!(0x13010820; sbfm(0, R0, R1, 1, 2));
        assert_emit!(0x93431062; sbfm(1, R2, R3, 3, 4));
        assert_emit!(0x53001c20; uxtb(R0, R1));
    }

    #[test]
    fn test_ldst_pair_pre() {
        assert_emit!(0xa9be7bfd; stp_pre(1, REG_FP, REG_LR, REG_SP, -4));
        assert_emit!(0x29840440; stp_pre(0, R0, R1, R2, 8));
    }

    #[test]
    fn test_ldst_pair_post() {
        assert_emit!(0xa8fe7bfd; ldp_post(1, REG_FP, REG_LR, REG_SP, -4));
        assert_emit!(0x28c40440; ldp_post(0, R0, R1, R2, 8));
    }

    #[test]
    fn test_count_empty_half_words() {
        assert_eq!(4, count_empty_half_words(0, 64));
        assert_eq!(3, count_empty_half_words(1, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 16, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 32, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 48, 64));

        assert_eq!(2, count_empty_half_words(0, 32));
        assert_eq!(1, count_empty_half_words(1, 32));
        assert_eq!(1, count_empty_half_words(1u64 << 16, 32));
    }

    #[test]
    fn test_fits_movz() {
        assert!(fits_movz(1, 64));
        assert!(fits_movz(0xFFFF, 64));
        assert!(fits_movz(0xFFFFu64 << 16, 64));
        assert!(!fits_movz(0x1FFFF, 64));
        assert!(fits_movz(1u64 << 16, 64));
        assert!(!fits_movz(0x10001, 64));

        assert!(fits_movz(1, 32));
        assert!(fits_movz(0xFFFF, 32));
        assert!(fits_movz(0xFFFFu64 << 16, 32));
        assert!(!fits_movz(0x1FFFF, 32));
    }

    #[test]
    fn test_fits_movn() {
        let min1 = !0u64;

        assert!(fits_movn(min1, 64));
        assert!(fits_movn(min1, 32));
        assert!(fits_movn(min1 << 2, 32));
        assert!(fits_movn(min1 << 16, 32));
        assert!(!fits_movn(min1 << 17, 32));
    }

    #[test]
    fn test_shift_movz() {
        assert_eq!(0, shift_movz(0));
        assert_eq!(0, shift_movz(1));
        assert_eq!(0, shift_movz(0xFFFF));
        assert_eq!(1, shift_movz(0x10000));
        assert_eq!(2, shift_movz(0x100000000));
    }

    #[test]
    fn test_shift_movn() {
        assert_eq!(0, shift_movn(!0u64));
        assert_eq!(0, shift_movn(0));
        assert_eq!(0, shift_movn(1));
        assert_eq!(1, shift_movn(0xFFFF));
        assert_eq!(2, shift_movn(0xFFFFFFFF));
    }

    #[test]
    fn test_add_extreg() {
        assert_eq!(0x8b22643f, add_extreg(1, REG_SP, R1, R2, Extend::UXTX, 1));
        assert_eq!(0x8b226be1, add_extreg(1, R1, REG_SP, R2, Extend::UXTX, 2));
        assert_eq!(0x0b22443f, add_extreg(0, REG_SP, R1, R2, Extend::UXTW, 1));
        assert_eq!(0x0b2243e1, add_extreg(0, R1, REG_SP, R2, Extend::UXTW, 0));
    }

    #[test]
    fn test_fp_dataproc2() {
        assert_eq!(0x1e222820, fadd(0, F0, F1, F2));
        assert_eq!(0x1e622820, fadd(1, F0, F1, F2));
        assert_eq!(0x1e653883, fsub(1, F3, F4, F5));
        assert_eq!(0x1e6808e6, fmul(1, F6, F7, F8));
        assert_eq!(0x1e6b1949, fdiv(1, F9, F10, F11));
    }

    #[test]
    fn test_fp_dataproc1() {
        assert_eq!(0x1e214041, fneg(0, F1, F2));
        assert_eq!(0x1e614083, fneg(1, F3, F4));
    }

    #[test]
    fn test_scvtf() {
        assert_eq!(0x1e220041, scvtf(0, 0, F1, R2));
        assert_eq!(0x1e620041, scvtf(0, 1, F1, R2));
        assert_eq!(0x9e220083, scvtf(1, 0, F3, R4));
        assert_eq!(0x9e620083, scvtf(1, 1, F3, R4));
    }

    #[test]
    fn test_uxtw() {
        assert_eq!(0xD3407c00, uxtw(R0, R0));
        assert_eq!(0xD3407d8f, uxtw(R15, R12));
    }

    #[test]
    fn test_sxtw() {
        assert_eq!(0x93407c00, sxtw(R0, R0));
        assert_eq!(0x93407d8f, sxtw(R15, R12));
    }

    #[test]
    fn test_fcmp() {
        assert_eq!(0x1e212000, fcmp(0, F0, F1));
        assert_eq!(0x1e612000, fcmp(1, F0, F1));
        assert_eq!(0x1e252080, fcmp(0, F4, F5));
    }

    #[test]
    fn test_fcmpe() {
        assert_eq!(0x1e212010, fcmpe(0, F0, F1));
        assert_eq!(0x1e612010, fcmpe(1, F0, F1));
        assert_eq!(0x1e252090, fcmpe(0, F4, F5));
    }

    #[test]
    fn test_fcvtzs() {
        assert_eq!(0x9e780020, fcvtzs(1, 1, R0, F1)); // x0, d1
        assert_eq!(0x9e380047, fcvtzs(1, 0, R7, F2)); // x7, s2
        assert_eq!(0x1e780020, fcvtzs(0, 1, R0, F1)); // w0, d1
        assert_eq!(0x1e380047, fcvtzs(0, 0, R7, F2)); // w7, s2
    }

    #[test]
    fn test_fsqrt() {
        assert_eq!(0x1e21c020, fsqrt(0, F0, F1)); // fsqrt s0, s1
        assert_eq!(0x1e61c020, fsqrt(1, F0, F1)); // fsqrt d0, d1
        assert_eq!(0x1e21c149, fsqrt(0, F9, F10)); // fsqrt s9, s10
        assert_eq!(0x1e61c149, fsqrt(1, F9, F10)); // fsqrt d9, d10
    }

    #[test]
    fn test_lsl_imm() {
        assert_eq!(0xd37ff820, lsl_imm(1, R0, R1, 1)); // lsl x0, x1, #1
        assert_eq!(0x531f7820, lsl_imm(0, R0, R1, 1)); // lsl w0, w1, #1
        assert_eq!(0xd37ef462, lsl_imm(1, R2, R3, 2)); // lsl x2, x3, #2
        assert_eq!(0x531e7462, lsl_imm(0, R2, R3, 2)); // lsl w2, w3, #2
    }

    #[test]
    fn test_lsr_imm() {
        assert_eq!(0xd341fc20, lsr_imm(1, R0, R1, 1)); // lsr x0, x1, #1
        assert_eq!(0x53017c20, lsr_imm(0, R0, R1, 1)); // lsr w0, w1, #1
        assert_eq!(0xd342fc62, lsr_imm(1, R2, R3, 2)); // lsr x2, x3, #2
        assert_eq!(0x53027c62, lsr_imm(0, R2, R3, 2)); // lsr w2, w3, #2
    }

    #[test]
    fn test_and_imm() {
        assert_eq!(0x12000020, and_imm(0, R0, R1, 1)); // and w0, w1, #1
        assert_eq!(0x92400020, and_imm(1, R0, R1, 1)); // and x0, x1, #1
        assert_eq!(0x1201f062, and_imm(0, R2, R3, 0xaaaaaaaa)); // and w2, w3, #aaaaaaaa

        // and x2, x3, #aaaaaaaaaaaaaaaa
        assert_eq!(0x9201f062, and_imm(1, R2, R3, 0xaaaaaaaaaaaaaaaa));
    }

    #[test]
    fn test_is_shifted_mask() {
        assert_eq!(true, is_shifted_mask(8));
        assert_eq!(true, is_shifted_mask(6));
        assert_eq!(true, is_shifted_mask(7));
        assert_eq!(true, is_shifted_mask(3));
        assert_eq!(true, is_shifted_mask(1));
        assert_eq!(true, is_shifted_mask(!0));

        assert_eq!(false, is_shifted_mask(0));
        assert_eq!(false, is_shifted_mask(9));
        assert_eq!(false, is_shifted_mask(1 << 63 | 1));
    }

    #[test]
    fn test_is_mask() {
        assert_eq!(true, is_mask(7));
        assert_eq!(true, is_mask(3));
        assert_eq!(true, is_mask(1));

        assert_eq!(false, is_mask(0));
        assert_eq!(false, is_mask(8));

        assert_eq!(true, is_mask(!0));
    }

    #[test]
    fn test_encode_logical_imm() {
        assert_eq!(None, encode_logical_imm(0, 64));
        assert_eq!(None, encode_logical_imm(!0, 64));

        assert_eq!(None, encode_logical_imm(0, 32));
        assert_eq!(None, encode_logical_imm((1 << 32) - 1, 32));

        assert_eq!(Some(0b0_000000_111100), encode_logical_imm(0x55555555, 32));
        assert_eq!(
            Some(0b0_000000_111100),
            encode_logical_imm(0x5555555555555555, 64)
        );
        assert_eq!(Some(0b0_000001_111100), encode_logical_imm(0xaaaaaaaa, 32));
        assert_eq!(
            Some(0b0_000001_111100),
            encode_logical_imm(0xaaaaaaaaaaaaaaaa, 64)
        );
        assert_eq!(Some(0b0_000000_111000), encode_logical_imm(0x11111111, 32));
        assert_eq!(Some(0b0_000001_111000), encode_logical_imm(0x88888888, 32));
        assert_eq!(Some(0b0_000001_111001), encode_logical_imm(0x99999999, 32));
    }

    fn asm(op1: u32, op2: u32) {
        if op1 != op2 {
            println!("{:x} != {:x}", op1, op2);
            panic!("insn do not match")
        }

        assert_eq!(op1, op2);
    }
}
