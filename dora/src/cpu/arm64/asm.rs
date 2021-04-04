use crate::asm::arm64::{Cond, Shift};
use crate::cpu::Reg;
use crate::masm::CondCode;

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
    use crate::cpu::arm64::reg::*;

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
