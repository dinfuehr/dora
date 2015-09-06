use codegen::buffer::Buffer;
use super::reg::Reg;

pub fn emit_movl_imm_reg(buf: &mut Buffer, val: u32, reg: Reg) {
    emit_op(buf, (0xB8 as u8) + reg.and7());
    buf.emit_u32(val);
}

pub fn emit_movq_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_rex(buf, 1, dest.msb(), 0, src.msb());
    emit_op(buf, 0x89);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
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
    assert!(mod_ >= 0 && mod_ <= 3);
    assert!(reg >= 0 && reg <= 7);
    assert!(rm >= 0 && rm <= 7);

    buf.emit_u8(mod_ << 6 | reg << 3 | rm);
}
