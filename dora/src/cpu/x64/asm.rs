use crate::cpu::*;
use crate::masm::MacroAssembler;

pub const VEXM_0F: u8 = 1;
pub const VEXM_0F38: u8 = 2;
pub const VEXM_0F3A: u8 = 3;

pub const VEXL_Z: u8 = 0;
pub const VEXL_128: u8 = 0;
pub const VEXL_256: u8 = 1;

pub const VEXP_NONE: u8 = 0;
pub const VEXP_66: u8 = 1;
pub const VEXP_F3: u8 = 2;
pub const VEXP_F2: u8 = 3;

fn emit_u8(buf: &mut MacroAssembler, val: u8) {
    buf.emit_u8(val)
}

fn emit_vex3_rxbm(buf: &mut MacroAssembler, r: u8, x: u8, b: u8, m: u8) {
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);
    assert!(m > 1 && m < 4);

    buf.emit_u8(0b11000100);
    buf.emit_u8(!r << 7 | (!x << 6) & 0b1000000 | (!b << 5) & 0b100000 | m);
}

fn emit_vex3_wvlp(buf: &mut MacroAssembler, w: bool, v: u8, l: u8, p: u8) {
    assert!(v < 16);
    assert!(l == 0 || l == 1);
    assert!(p < 4);

    buf.emit_u8((w as u8) << 7 | (!v << 3) & 0b1111000 | l << 2 | p);
}

fn emit_modrm(buf: &mut MacroAssembler, mode: u8, reg: u8, rm: u8) {
    assert!(mode < 4);
    assert!(reg < 8);
    assert!(rm < 8);

    buf.emit_u8(mode << 6 | reg << 3 | rm);
}

pub fn emit_shlx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_66);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

pub fn emit_shrx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_F2);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

pub fn emit_sarx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_F3);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::masm::MacroAssembler;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = MacroAssembler::new();
            $name(&mut buf);
            let expected = vec![$($expr,)*];

            assert_eq!(expected, buf.data());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            )
        ) => {{
            let mut buf = MacroAssembler::new();
            $name(&mut buf, $($param,)*);
            let expected = vec![$($expr,)*];
            let data = buf.data();

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
    fn test_int_shlx() {
        assert_emit!(0xC4, 0xE2, 0xE1, 0xF7, 0xC2; emit_shlx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x29, 0xF7, 0xC1; emit_shlx(false, R8, R9, R10));
    }

    #[test]
    fn test_int_shrx() {
        assert_emit!(0xC4, 0xE2, 0xE3, 0xF7, 0xC2; emit_shrx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x2B, 0xF7, 0xC1; emit_shrx(false, R8, R9, R10));
    }

    #[test]
    fn test_int_sarx() {
        assert_emit!(0xC4, 0xE2, 0xE2, 0xF7, 0xC2; emit_sarx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x22, 0xF7, 0xCA; emit_sarx(false, R9, R10, R11));
    }
}
