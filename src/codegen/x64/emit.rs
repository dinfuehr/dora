use codegen::buffer::Buffer;
use super::reg::Reg;
use super::reg::Reg::*;

pub fn emit_movl_imm_reg(buf: &mut Buffer, val: u32, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, (0xB8 as u8) + reg.and7());
    emit_u32(buf, val);
}

pub fn emit_subq_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg) {
    emit_rex(buf, 1, 0, 0, reg.msb());

    if fits_i8(imm) {
        emit_op(buf, 0x83);
        emit_modrm(buf, 0b11, 0b101, reg.and7());
        emit_u8(buf, imm as u8);
    } else if reg == RAX {
        emit_op(buf, 0x2d);
        emit_u32(buf, imm as u32);
    } else {
        emit_op(buf, 0x81);
        emit_modrm(buf, 0b11, 0b101, reg.and7());
        emit_u32(buf, imm as u32);
    }
}

pub fn emit_movq_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_rex(buf, 1, src.msb(), 0, dest.msb());
    emit_op(buf, 0x89);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_pushq_reg(buf: &mut Buffer, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, 0x50 + reg.and7());
}

pub fn emit_popq_reg(buf: &mut Buffer, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, 0x58 + reg.and7());
}

pub fn emit_retq(buf: &mut Buffer) {
    emit_op(buf, 0xC3);
}

pub fn emit_u32(buf: &mut Buffer, val: u32) {
    buf.emit_u32(val)
}

pub fn emit_u8(buf: &mut Buffer, val: u8) {
    buf.emit_u8(val)
}

pub fn emit_op(buf: &mut Buffer, opcode: u8) {
    buf.emit_u8(opcode);
}

fn emit_rex(buf: &mut Buffer, w: u8, r: u8, x: u8, b: u8) {
    assert!(w == 0 || w == 1);
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);

    buf.emit_u8(0x4 << 4 | w << 3 | r << 2 | x << 1 | b);
}

fn emit_modrm(buf: &mut Buffer, mod_: u8, reg: u8, rm: u8) {
    assert!(mod_ < 4);
    assert!(reg < 8);
    assert!(rm < 8);

    buf.emit_u8(mod_ << 6 | reg << 3 | rm);
}

fn emit_sib(buf: &mut Buffer, scale: u8, index: u8, base: u8) {
    assert!(scale < 4);
    assert!(index < 4);
    assert!(base < 8);

    assert!(index != RSP.int() && index != R12.int());
    buf.emit_u8(scale << 6 | index << 3 | base);
}

pub fn fits_i8(imm: i32) -> bool {
    imm == (imm as i8) as i32
}

#[cfg(test)]
mod tests {
    use codegen::buffer::Buffer;
    use codegen::x64::reg::Reg::*;
    use super::*;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = Buffer::new();
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
            let mut buf = Buffer::new();
            $name(&mut buf, $($param,)*);
            let expected = vec![$($expr,)*];

            assert_eq!(expected, buf.data());
        }};
    }

    #[test]
    fn test_fits8() {
        assert!(fits_i8(1));
        assert!(fits_i8(0));
        assert!(fits_i8(-1));
        assert!(fits_i8(127));
        assert!(fits_i8(-128));

        assert!(!fits_i8(128));
        assert!(!fits_i8(-129));
    }

    #[test]
    fn test_emit_retq() {
        assert_emit!(0xc3; emit_retq);
    }

    #[test]
    fn test_emit_popq_reg() {
        assert_emit!(0x58; emit_popq_reg(RAX));
        assert_emit!(0x5c; emit_popq_reg(RSP));
        assert_emit!(0x41, 0x58; emit_popq_reg(R8));
        assert_emit!(0x41, 0x5F; emit_popq_reg(R15));
    }

    #[test]
    fn test_emit_pushq_reg() {
        assert_emit!(0x50; emit_pushq_reg(RAX));
        assert_emit!(0x54; emit_pushq_reg(RSP));
        assert_emit!(0x41, 0x50; emit_pushq_reg(R8));
        assert_emit!(0x41, 0x57; emit_pushq_reg(R15));
    }

    #[test]
    fn test_emit_movq_reg_reg() {
        assert_emit!(0x4c, 0x89, 0xf8; emit_movq_reg_reg(R15, RAX));
        assert_emit!(0x49, 0x89, 0xc7; emit_movq_reg_reg(RAX, R15));
    }

    #[test]
    fn test_emit_movl_imm_reg() {
        assert_emit!(0xb8, 2, 0, 0, 0; emit_movl_imm_reg(2, RAX));
        assert_emit!(0x41, 0xbe, 3, 0, 0, 0; emit_movl_imm_reg(3, R14));
    }

    #[test]
    fn test_emit_subq_imm_reg() {
        assert_emit!(0x48, 0x83, 0xe8, 0x11; emit_subq_imm_reg(0x11, RAX));
        assert_emit!(0x49, 0x83, 0xef, 0x11; emit_subq_imm_reg(0x11, R15));
        assert_emit!(0x48, 0x2d, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, RAX));
        assert_emit!(0x48, 0x81, 0xe9, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, RCX));
        assert_emit!(0x49, 0x81, 0xef, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, R15));
    }
}
