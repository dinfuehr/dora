use codegen::buffer::*;
use super::reg::Reg;
use super::reg::Reg::*;

pub fn emit_movl_imm_reg(buf: &mut Buffer, imm: u32, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, (0xB8 as u8) + reg.and7());
    emit_u32(buf, imm);
}

pub fn emit_movl_memq_reg(buf: &mut Buffer, src: Reg, disp: i32, dest: Reg) {
    if (src != RIP && src.msb() != 0) || dest.msb() != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x8b);
    emit_membase(buf, src, disp, dest);
}

pub fn emit_movl_reg_memq(buf: &mut Buffer, src: Reg, dest: Reg, disp: i32) {
    if (dest != RIP && dest.msb() != 0) || src.msb() != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x89);
    emit_membase(buf, dest, disp, src);
}

fn emit_membase(buf: &mut Buffer, base: Reg, disp: i32, dest: Reg) {
    if base == RSP || base == R12 {
        if disp == 0 {
            emit_modrm(buf, 0, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
        } else if fits_i8(disp) {
            emit_modrm(buf, 1, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
            emit_u8(buf, disp as u8);
        } else {
            emit_modrm(buf, 2, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
            emit_u32(buf, disp as u32);
        }

    } else if disp == 0 && base != RBP && base != R13 && base != RIP {
        emit_modrm(buf, 0, dest.and7(), base.and7());

    } else if base == RIP {
        emit_modrm(buf, 0, dest.and7(), RBP.and7());
        emit_u32(buf, disp as u32);

    } else if fits_i8(disp) {
        emit_modrm(buf, 1, dest.and7(), base.and7());
        emit_u8(buf, disp as u8);

    } else {
        emit_modrm(buf, 2, dest.and7(), base.and7());
        emit_u32(buf, disp as u32);
    }
}

pub fn emit_subq_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg) {
    emit_alu_imm_reg(buf, imm, reg, 0x2d, 0b101);
}

pub fn emit_addq_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg) {
    emit_alu_imm_reg(buf, imm, reg, 0x05, 0);
}

fn emit_alu_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg, rax_opcode: u8, modrm_reg: u8) {
    emit_rex(buf, 1, 0, 0, reg.msb());

    if fits_i8(imm) {
        emit_op(buf, 0x83);
        emit_modrm(buf, 0b11, modrm_reg, reg.and7());
        emit_u8(buf, imm as u8);
    } else if reg == RAX {
        emit_op(buf, rax_opcode);
        emit_u32(buf, imm as u32);
    } else {
        emit_op(buf, 0x81);
        emit_modrm(buf, 0b11, modrm_reg, reg.and7());
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

pub fn emit_nop(buf: &mut Buffer) {
    emit_op(buf, 0x90);
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

fn emit_modrm(buf: &mut Buffer, mode: u8, reg: u8, rm: u8) {
    assert!(mode < 4);
    assert!(reg < 8);
    assert!(rm < 8);

    buf.emit_u8(mode << 6 | reg << 3 | rm);
}

fn emit_sib(buf: &mut Buffer, scale: u8, index: u8, base: u8) {
    assert!(scale < 4);
    assert!(index < 8);
    assert!(base < 8);

    buf.emit_u8(scale << 6 | index << 3 | base);
}

pub fn fits_i8(imm: i32) -> bool {
    imm == (imm as i8) as i32
}

pub fn emit_jz(buf: &mut Buffer, lbl: Label) {
    emit_op(buf, 0x0f);
    emit_op(buf, 0x84);
    buf.emit_label(lbl);
}

pub fn emit_jmp(buf: &mut Buffer, lbl: Label) {
    emit_op(buf, 0xe9);
    buf.emit_label(lbl);
}

pub fn emit_testl_reg_reg(buf: &mut Buffer, op1: Reg, op2: Reg) {
    assert!(op1.msb() == 0);
    assert!(op2.msb() == 0);

    emit_op(buf, 0x85);
    emit_modrm(buf, 0b11, op1.and7(), op2.and7());
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

            assert_eq!(expected, buf.finish());
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

            assert_eq!(expected, buf.finish());
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

    #[test]
    fn test_emit_addq_imm_reg() {
        assert_emit!(0x48, 0x83, 0xc0, 0x11; emit_addq_imm_reg(0x11, RAX));
        assert_emit!(0x49, 0x83, 0xc7, 0x11; emit_addq_imm_reg(0x11, R15));
        assert_emit!(0x48, 0x05, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, RAX));
        assert_emit!(0x48, 0x81, 0xc1, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, RCX));
        assert_emit!(0x49, 0x81, 0xc7, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, R15));
    }

    #[test]
    fn test_emit_testl_reg_reg() {
        assert_emit!(0x85, 0xc0; emit_testl_reg_reg(RAX, RAX));
        assert_emit!(0x85, 0xc6; emit_testl_reg_reg(RAX, RSI));
    }

    #[test]
    fn test_emit_jz() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        emit_jz(&mut buf, lbl);
        emit_nop(&mut buf);
        buf.define_label(lbl);
        assert_eq!(vec![0x0f, 0x84, 1, 0, 0, 0, 0x90], buf.finish());
    }

    #[test]
    fn test_emit_jmp() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        emit_jmp(&mut buf, lbl);
        emit_nop(&mut buf);
        buf.define_label(lbl);
        assert_eq!(vec![0xe9, 1, 0, 0, 0, 0x90], buf.finish());
    }

    #[test]
    fn test_emit_movl_memq_reg() {
        assert_emit!(0x8b, 0x44, 0x24, 1; emit_movl_memq_reg(RSP, 1, RAX));
        assert_emit!(0x8b, 0x04, 0x24; emit_movl_memq_reg(RSP, 0, RAX));

        assert_emit!(0x44, 0x8b, 0x45, 0; emit_movl_memq_reg(RBP, 0, R8));

        assert_emit!(0x8b, 0x05, 0, 0, 0, 0; emit_movl_memq_reg(RIP, 0, RAX));
        assert_emit!(0x8b, 0x0d, 0, 0, 0, 0; emit_movl_memq_reg(RIP, 0, RCX));
    }

    #[test]
    fn test_emit_movl_reg_memq() {
        assert_emit!(0x89, 0x0d, 0, 0, 0, 0; emit_movl_reg_memq(RCX, RIP, 0));
        assert_emit!(0x89, 0x48, 3; emit_movl_reg_memq(RCX, RAX, 3));
    }
}
