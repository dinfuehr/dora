use cpu::Reg;
use jit::buffer::Buffer;

pub fn nop(buf: &mut Buffer) {
    buf.emit_u32(0);
}

pub fn add_reg(x: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    assert!(fits_bit(x));

    // x000 1011 xx0x xxxx xxxx xxnn nnnd dddd
    0x0B000000 | rd.u32() | rn.u32() << 5 | rm.u32() << 16 | x << 31
}

pub fn sub_reg(x: u32, rd: Reg, rn: Reg, rm: Reg) -> u32 {
    assert!(fits_bit(x));

    // x000 1011 xx0x xxxx xxxx xxnn nnnd dddd
    0x4B000000 | rd.u32() | rn.u32() << 5 | rm.u32() << 16 | x << 31
}

// fn class_addsub(rd: i32, rn: i32, imm: i32) -> u32 {
//     assert!(fits_i5(rd));
//     assert!(fits_i5(rn));
//     assert!(fits_i12(imm));
// }

fn fits_bit(imm: u32) -> bool {
    imm == 0 || imm == 1
}

fn fits_u5(imm: u32) -> bool {
    imm < 32
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
    fn test_fits_u5() {
        assert!(fits_u5(0));
        assert!(fits_u5(31));
        assert!(!fits_u5(32));
        assert!(!fits_u5(33));
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