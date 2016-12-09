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
    cls_addsub_shreg(sf, 0, 0, Shift::Lsl, rm, 0, rn, rd)
}

pub fn sub_reg(sf: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    cls_addsub_shreg(sf, 1, 0, Shift::Lsl, rm, 0, rn, rd)
}

pub fn add_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 0, 0, shift, rm, amount, rn, rd)
}

pub fn sub_shreg(sf: u32, rd: Reg, rn: Reg, rm: Reg, shift: Shift, amount: u32) -> u32 {
    cls_addsub_shreg(sf, 1, 0, shift, rm, amount, rn, rd)
}

fn cls_addsub_shreg(sf: u32, op: u32, s: u32, shift: Shift, rm: Reg,
                    imm6: u32, rn: Reg, rd: Reg) -> u32 {
    assert!(fits_bit(sf));
    assert!(fits_bit(op));
    assert!(fits_bit(s));
    assert!(rm.is_gpr());
    assert!(rn.is_gpr());
    assert!(rd.is_gpr());
    assert!(fits_u5(imm6));

    (0b01011 as u32) << 24 | sf << 31 | op << 30 | s << 29 |
        shift.u32() << 22 | rm.u32() << 16 | imm6 << 10 | rn.u32() << 5 | rd.u32()
}

#[derive(Copy, Clone)]
pub enum Shift {
    Lsl, Lsr, Asr
}

impl Shift {
    fn u32(self) -> u32 {
        match self {
            Shift::Lsl => 0,
            Shift::Lsr => 1,
            Shift::Asr => 2,
        }
    }
}

fn fits_bit(imm: u32) -> bool {
    imm < 2
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

fn fits_u12(imm: u32) -> bool {
    imm < 4096
}

fn fits_i12(imm: i32) -> bool {
    -2048 <= imm && imm < 2048
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
//    4:	91000420 	add	x0, x1, #0x1
//    8:	91400c62 	add	x2, x3, #0x3, lsl #12

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
        assert_emit!(0x0b030441; add_shreg(0, R1, R2, R3, Shift::Lsl, 1));
        assert_emit!(0x8b0608a4; add_shreg(1, R4, R5, R6, Shift::Lsl, 2));
        assert_emit!(0x0b430441; add_shreg(0, R1, R2, R3, Shift::Lsr, 1));
        assert_emit!(0x8b8608a4; add_shreg(1, R4, R5, R6, Shift::Asr, 2));
    }

    #[test]
    fn test_sub_shreg() {
        assert_emit!(0x4b030441; sub_shreg(0, R1, R2, R3, Shift::Lsl, 1));
        assert_emit!(0xcb0608a4; sub_shreg(1, R4, R5, R6, Shift::Lsl, 2));
        assert_emit!(0x4b430441; sub_shreg(0, R1, R2, R3, Shift::Lsr, 1));
        assert_emit!(0xcb8608a4; sub_shreg(1, R4, R5, R6, Shift::Asr, 2));
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
}