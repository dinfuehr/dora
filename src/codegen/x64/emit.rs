use codegen::buffer::*;
use parser::ast::CmpOp;
use super::reg::Reg;
use super::reg::Reg::*;

pub fn emit_orl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, 0, 0x09, src, dest);
}

pub fn emit_andl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, 0, 0x21, src, dest);
}

pub fn emit_xorl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, 0, 0x31, src, dest);
}

fn emit_alu_reg_reg(buf: &mut Buffer, x64: u8, opcode: u8, src: Reg, dest: Reg) {
    if x64 != 0 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_movl_imm_reg(buf: &mut Buffer, imm: u32, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, (0xB8 as u8) + reg.and7());
    emit_u32(buf, imm);
}

pub fn emit_movb_memq_reg(buf: &mut Buffer, src: Reg, disp: i32, dest: Reg) {
    let rex_prefix = if dest != RAX && dest != RBX && dest != RCX && dest != RDX { 1 } else { 0 };

    emit_mov_memq_reg(buf, rex_prefix, 0, 0x8a, src, disp, dest);
}

pub fn emit_movl_memq_reg(buf: &mut Buffer, src: Reg, disp: i32, dest: Reg) {
    emit_mov_memq_reg(buf, 0, 0, 0x8b, src, disp, dest);
}

pub fn emit_movq_memq_reg(buf: &mut Buffer, src: Reg, disp: i32, dest: Reg) {
    emit_mov_memq_reg(buf, 0, 1, 0x8b, src, disp, dest);
}

fn emit_mov_memq_reg(buf: &mut Buffer, rex_prefix: u8, x64: u8,
        opcode: u8, src: Reg, disp: i32, dest: Reg) {
    let src_msb = if src == RIP { 0 } else { src.msb() };

    if src_msb != 0 || dest.msb() != 0 || x64 != 0 || rex_prefix != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, src, disp, dest);
}

pub fn emit_movq_reg_memq(buf: &mut Buffer, src: Reg, dest: Reg, disp: i32) {
    emit_mov_reg_memq(buf, 0x89, 1, src, dest, disp);
}

pub fn emit_movl_reg_memq(buf: &mut Buffer, src: Reg, dest: Reg, disp: i32) {
    emit_mov_reg_memq(buf, 0x89, 0, src, dest, disp);
}

pub fn emit_movb_reg_memq(buf: &mut Buffer, src: Reg, dest: Reg, disp: i32) {
    let dest_msb = if dest == RIP { 0 } else { dest.msb() };

    if dest_msb != 0 || src.msb() != 0 ||
        (src != RAX && src != RBX && src != RCX && src != RDX) {
        emit_rex(buf, 0, dest_msb, 0, src.msb());
    }

    emit_op(buf, 0x88);
    emit_membase(buf, dest, disp, src);
}

fn emit_mov_reg_memq(buf: &mut Buffer, opcode: u8, x64: u8, src: Reg, dest: Reg, disp: i32) {
    let dest_msb = if dest == RIP { 0 } else { dest.msb() };

    if dest_msb != 0 || src.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, dest_msb, 0, src.msb());
    }

    emit_op(buf, opcode);
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
    emit_aluq_imm_reg(buf, imm, reg, 0x2d, 0b101);
}

pub fn emit_addq_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg) {
    emit_aluq_imm_reg(buf, imm, reg, 0x05, 0);
}

fn emit_aluq_imm_reg(buf: &mut Buffer, imm: i32, reg: Reg, rax_opcode: u8, modrm_reg: u8) {
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
    emit_mov_reg_reg(buf, 1, src, dest);
}

pub fn emit_movl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_mov_reg_reg(buf, 0, src, dest);
}

fn emit_mov_reg_reg(buf: &mut Buffer, x64: u8, src: Reg, dest: Reg) {
    if x64 != 0 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x89);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_negl_reg(buf: &mut Buffer, reg: Reg) {
    emit_alul_reg(buf, 0xf7, 0b11, 0, reg);
}

pub fn emit_notl_reg(buf: &mut Buffer, reg: Reg) {
    emit_alul_reg(buf, 0xf7, 0b10, 0, reg);
}

fn emit_alul_reg(buf: &mut Buffer, opcode: u8, modrm_reg: u8, x64: u8, reg: Reg) {
    if reg.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, reg.msb());
    }

    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, modrm_reg, reg.and7());
}

pub fn emit_xorb_imm_reg(buf: &mut Buffer, imm: u8, dest: Reg) {
    emit_alub_imm_reg(buf, 0x80, 0x34, 0b110, imm, dest);
}

pub fn emit_andb_imm_reg(buf: &mut Buffer, imm: u8, dest: Reg) {
    emit_alub_imm_reg(buf, 0x80, 0x24, 0b100, imm, dest);
}

fn emit_alub_imm_reg(buf: &mut Buffer, opcode: u8, rax_opcode: u8,
                     modrm_reg: u8, imm: u8, dest: Reg) {
    if dest == RAX {
        emit_op(buf, rax_opcode);
        emit_u8(buf, imm);
    } else {
        if dest.msb() != 0 || !dest.is_basic_reg() {
            emit_rex(buf, 0, 0, 0, dest.msb());
        }

        emit_op(buf, opcode);
        emit_modrm(buf, 0b11, modrm_reg, dest.and7());
        emit_u8(buf, imm);
    }
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

pub fn emit_jnz(buf: &mut Buffer, lbl: Label) {
    emit_op(buf, 0x0f);
    emit_op(buf, 0x85);
    buf.emit_label(lbl);
}

pub fn emit_jmp(buf: &mut Buffer, lbl: Label) {
    emit_op(buf, 0xe9);
    buf.emit_label(lbl);
}

pub fn emit_testl_reg_reg(buf: &mut Buffer, op1: Reg, op2: Reg) {
    if op1.msb() != 0 || op2.msb() != 0 {
        emit_rex(buf, 0, op1.msb(), 0, op2.msb());
    }

    emit_op(buf, 0x85);
    emit_modrm(buf, 0b11, op1.and7(), op2.and7());
}

pub fn emit_addl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, 0, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x01);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_subl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, 0, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x29);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_imull_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xaf);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_idivl_reg_reg(buf: &mut Buffer, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, reg.msb());
    }

    emit_op(buf, 0xf7);
    emit_modrm(buf, 0b11, 0b111, reg.and7());
}

pub fn emit_cmpl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, 0, 0x39, src, dest);
}

pub fn emit_cltd(buf: &mut Buffer) {
    emit_op(buf, 0x99);
}

pub fn emit_setb_reg(buf: &mut Buffer, op: CmpOp, reg: Reg) {
    if reg.msb() != 0 || !reg.is_basic_reg() {
        emit_rex(buf, 0, 0, 0, reg.msb());
    }

    let op = match op {
        CmpOp::Lt => 0x9c,
        CmpOp::Le => 0x9e,
        CmpOp::Gt => 0x9f,
        CmpOp::Ge => 0x9d,
        CmpOp::Eq => 0x94,
        CmpOp::Ne => 0x95,
    };

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, 0, reg.and7());
}

pub fn emit_movb_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || !src.is_basic_reg() {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x88);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_movzbl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || !src.is_basic_reg() {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xb6);

    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_cmpb_imm_reg(buf: &mut Buffer, imm: u8, dest: Reg) {
    if dest == RAX {
        emit_op(buf, 0x3c);
        emit_u8(buf, imm);
        return;
    }

    if dest.msb() != 0 || !dest.is_basic_reg() {
        emit_rex(buf, 0, 0, 0, dest.msb());
    }

    emit_op(buf, 0x80);
    emit_modrm(buf, 0b11, 0b111, dest.and7());
    emit_u8(buf, imm);
}

pub fn emit_callq_reg(buf: &mut Buffer, dest: Reg) {
    if dest.msb() != 0 {
        emit_rex(buf, 0, 0, 0, dest.msb());
    }

    emit_op(buf, 0xff);
    emit_modrm(buf, 0b11, 0b10, dest.and7());
}

#[cfg(test)]
mod tests {
    use codegen::buffer::Buffer;
    use codegen::x64::reg::Reg::*;
    use parser::ast::CmpOp;
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
    fn test_emit_movl_reg_reg() {
        assert_emit!(0x44, 0x89, 0xf8; emit_movl_reg_reg(R15, RAX));
        assert_emit!(0x41, 0x89, 0xc7; emit_movl_reg_reg(RAX, R15));
        assert_emit!(0x89, 0xc8; emit_movl_reg_reg(RCX, RAX));
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
        assert_emit!(0x41, 0x85, 0xc7; emit_testl_reg_reg(RAX, R15));
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
    fn test_emit_jnz() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        emit_jnz(&mut buf, lbl);
        emit_nop(&mut buf);
        buf.define_label(lbl);
        assert_eq!(vec![0x0f, 0x85, 1, 0, 0, 0, 0x90], buf.finish());
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
    fn test_emit_movq_memq_reg() {
        assert_emit!(0x48, 0x8b, 0x44, 0x24, 1; emit_movq_memq_reg(RSP, 1, RAX));

        assert_emit!(0x48, 0x8b, 0x05, 0xff, 0xff, 0xff, 0xff; emit_movq_memq_reg(RIP, -1, RAX));
        assert_emit!(0x48, 0x8b, 0x05, 0, 0, 0, 0; emit_movq_memq_reg(RIP, 0, RAX));
        assert_emit!(0x48, 0x8b, 0x05, 1, 0, 0, 0; emit_movq_memq_reg(RIP, 1, RAX));
    }

    #[test]
    fn test_emit_movb_memq_reg() {
        assert_emit!(0x8a, 0x45, 1; emit_movb_memq_reg(RBP, 1, RAX));
        assert_emit!(0x8a, 0x44, 0x24, 1; emit_movb_memq_reg(RSP, 1, RAX));
        assert_emit!(0x8a, 0x44, 0x24, 0xff; emit_movb_memq_reg(RSP, -1, RAX));
        assert_emit!(0x8a, 0x5d, 1; emit_movb_memq_reg(RBP, 1, RBX));
        assert_emit!(0x8a, 0x4d, 1; emit_movb_memq_reg(RBP, 1, RCX));
        assert_emit!(0x8a, 0x55, 1; emit_movb_memq_reg(RBP, 1, RDX));
        assert_emit!(0x44, 0x8a, 0x7d, 1; emit_movb_memq_reg(RBP, 1, R15));
        assert_emit!(0x40, 0x8a, 0x75, 1; emit_movb_memq_reg(RBP, 1, RSI));
        assert_emit!(0x40, 0x8a, 0x7d, 1; emit_movb_memq_reg(RBP, 1, RDI));
    }

    #[test]
    fn test_emit_movl_reg_memq() {
        assert_emit!(0x89, 0x0d, 0, 0, 0, 0; emit_movl_reg_memq(RCX, RIP, 0));
        assert_emit!(0x89, 0x48, 3; emit_movl_reg_memq(RCX, RAX, 3));
    }

    #[test]
    fn test_emit_movq_reg_memq() {
        assert_emit!(0x48, 0x89, 0x0d, 0, 0, 0, 0; emit_movq_reg_memq(RCX, RIP, 0));
        assert_emit!(0x48, 0x89, 0x48, 3; emit_movq_reg_memq(RCX, RAX, 3));
    }

    #[test]
    fn test_emit_movb_reg_memq() {
        assert_emit!(0x88, 0x0d, 0, 0, 0, 0; emit_movb_reg_memq(RCX, RIP, 0));
        assert_emit!(0x88, 0x48, 3; emit_movb_reg_memq(RCX, RAX, 3));
        assert_emit!(0x40, 0x88, 0x75, 0xFF; emit_movb_reg_memq(RSI, RBP, -1));

        assert_emit!(0x88, 0x45, 0x01; emit_movb_reg_memq(RAX, RBP, 1));
        assert_emit!(0x88, 0x44, 0x24, 0x01; emit_movb_reg_memq(RAX, RSP, 1));
        assert_emit!(0x88, 0x5d, 0x01; emit_movb_reg_memq(RBX, RBP, 1));
        assert_emit!(0x88, 0x4d, 0x01; emit_movb_reg_memq(RCX, RBP, 1));
        assert_emit!(0x88, 0x55, 0x01; emit_movb_reg_memq(RDX, RBP, 1));
    }

    #[test]
    fn test_negl_reg() {
        assert_emit!(0xf7, 0xd8; emit_negl_reg(RAX));
        assert_emit!(0x41, 0xf7, 0xdf; emit_negl_reg(R15));
    }

    #[test]
    fn test_notl_reg() {
        assert_emit!(0xf7, 0xd0; emit_notl_reg(RAX));
        assert_emit!(0x41, 0xf7, 0xd7; emit_notl_reg(R15));
    }

    #[test]
    fn test_xorb_imm_reg() {
        assert_emit!(0x34, 1; emit_xorb_imm_reg(1, RAX));
        assert_emit!(0x80, 0xf3, 1; emit_xorb_imm_reg(1, RBX));
        assert_emit!(0x80, 0xf1, 1; emit_xorb_imm_reg(1, RCX));
        assert_emit!(0x80, 0xf2, 1; emit_xorb_imm_reg(1, RDX));
        assert_emit!(0x40, 0x80, 0xf6, 1; emit_xorb_imm_reg(1, RSI));
        assert_emit!(0x40, 0x80, 0xf7, 1; emit_xorb_imm_reg(1, RDI));

        assert_emit!(0x41, 0x80, 0xf0, 3; emit_xorb_imm_reg(3, R8));
        assert_emit!(0x41, 0x80, 0xf7, 4; emit_xorb_imm_reg(4, R15));
    }

    #[test]
    fn test_andb_imm_reg() {
        assert_emit!(0x24, 1; emit_andb_imm_reg(1, RAX));
        assert_emit!(0x80, 0xe1, 2; emit_andb_imm_reg(2, RCX));
        assert_emit!(0x41, 0x80, 0xe0, 3; emit_andb_imm_reg(3, R8));
        assert_emit!(0x41, 0x80, 0xe7, 4; emit_andb_imm_reg(4, R15));
    }

    #[test]
    fn test_addl_reg_reg() {
        assert_emit!(0x01, 0xd8; emit_addl_reg_reg(RBX, RAX));
        assert_emit!(0x44, 0x01, 0xf9; emit_addl_reg_reg(R15, RCX));
    }

    #[test]
    fn test_subl_reg_reg() {
        assert_emit!(0x29, 0xd8; emit_subl_reg_reg(RBX, RAX));
        assert_emit!(0x44, 0x29, 0xf9; emit_subl_reg_reg(R15, RCX));
    }

    #[test]
    fn test_imull_reg_reg() {
        assert_emit!(0x0f, 0xaf, 0xc3; emit_imull_reg_reg(RBX, RAX));
        assert_emit!(0x41, 0x0f, 0xaf, 0xcf; emit_imull_reg_reg(R15, RCX));
    }

    #[test]
    fn test_idivl_reg_reg() {
        assert_emit!(0xf7, 0xf8; emit_idivl_reg_reg(RAX));
        assert_emit!(0x41, 0xf7, 0xff; emit_idivl_reg_reg(R15));
    }

    #[test]
    fn test_cltd() {
        assert_emit!(0x99; emit_cltd);
    }

    #[test]
    fn test_orl_reg_reg() {
        assert_emit!(0x44, 0x09, 0xf8; emit_orl_reg_reg(R15, RAX));
        assert_emit!(0x09, 0xc8; emit_orl_reg_reg(RCX, RAX));
        assert_emit!(0x41, 0x09, 0xc7; emit_orl_reg_reg(RAX, R15));
    }

    #[test]
    fn test_andl_reg_reg() {
        assert_emit!(0x44, 0x21, 0xf8; emit_andl_reg_reg(R15, RAX));
        assert_emit!(0x21, 0xc8; emit_andl_reg_reg(RCX, RAX));
        assert_emit!(0x41, 0x21, 0xc7; emit_andl_reg_reg(RAX, R15));
    }

    #[test]
    fn test_xorl_reg_reg() {
        assert_emit!(0x44, 0x31, 0xf8; emit_xorl_reg_reg(R15, RAX));
        assert_emit!(0x31, 0xc8; emit_xorl_reg_reg(RCX, RAX));
        assert_emit!(0x41, 0x31, 0xc7; emit_xorl_reg_reg(RAX, R15));
    }

    #[test]
    fn test_cmpl_reg_reg() {
        assert_emit!(0x44, 0x39, 0xf8; emit_cmpl_reg_reg(R15, RAX));
        assert_emit!(0x41, 0x39, 0xdf; emit_cmpl_reg_reg(RBX, R15));
        assert_emit!(0x39, 0xd8; emit_cmpl_reg_reg(RBX, RAX));
    }

    #[test]
    fn test_setb_reg() {
        assert_emit!(0x0f, 0x94, 0xc0; emit_setb_reg(CmpOp::Eq, RAX));
        assert_emit!(0x41, 0x0f, 0x95, 0xc7; emit_setb_reg(CmpOp::Ne, R15));
        assert_emit!(0x0f, 0x9d, 0xc1; emit_setb_reg(CmpOp::Ge, RCX));
        assert_emit!(0x0f, 0x9f, 0xc2; emit_setb_reg(CmpOp::Gt, RDX));
        assert_emit!(0x40, 0x0f, 0x9e, 0xc6; emit_setb_reg(CmpOp::Le, RSI));
        assert_emit!(0x40, 0x0f, 0x9c, 0xc7; emit_setb_reg(CmpOp::Lt, RDI));
    }

    #[test]
    fn test_movb_reg_reg() {
        assert_emit!(0x88, 0xd8; emit_movb_reg_reg(RBX, RAX));
        assert_emit!(0x88, 0xd1; emit_movb_reg_reg(RDX, RCX));
        assert_emit!(0x45, 0x88, 0xd1; emit_movb_reg_reg(R10, R9));
        assert_emit!(0x40, 0x88, 0xfe; emit_movb_reg_reg(RDI, RSI));
        assert_emit!(0x45, 0x88, 0xf7; emit_movb_reg_reg(R14, R15));
    }

    #[test]
    fn test_movzbl_reg_reg() {
        assert_emit!(0x0f, 0xb6, 0xc0; emit_movzbl_reg_reg(RAX, RAX));
        assert_emit!(0x41, 0x0f, 0xb6, 0xc7; emit_movzbl_reg_reg(R15, RAX));
        assert_emit!(0x44, 0x0f, 0xb6, 0xfb; emit_movzbl_reg_reg(RBX, R15));
        assert_emit!(0x40, 0x0f, 0xb6, 0xce; emit_movzbl_reg_reg(RSI, RCX));
    }

    #[test]
    fn test_emit_cmpb_imm_reg() {
        assert_emit!(0x3c, 0; emit_cmpb_imm_reg(0, RAX));
        assert_emit!(0x80, 0xf9, 0; emit_cmpb_imm_reg(0, RCX));
        assert_emit!(0x41, 0x80, 0xff, 0; emit_cmpb_imm_reg(0, R15));
        assert_emit!(0x40, 0x80, 0xfe, 0; emit_cmpb_imm_reg(0, RSI));
    }

    #[test]
    fn test_callq_reg() {
        assert_emit!(0xff, 0xd0; emit_callq_reg(RAX));
        assert_emit!(0x41, 0xff, 0xd7; emit_callq_reg(R15));
    }
}
