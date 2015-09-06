use codegen::buffer::Buffer;
use super::reg::Reg;

pub fn emit_movl_imm_reg(buf: &mut Buffer, val: u32, reg: Reg) {
    emit_op(buf, (0xB8 as u8) + reg.and7());
    buf.emit_u32(val);
}

pub fn emit_movq_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {

}

pub fn emit_pushq_reg(buf: &mut Buffer, reg: Reg) {

}

pub fn emit_popq_reg(buf: &mut Buffer, reg: Reg) {
    // if reg.msb() != 0 {
    //     emit_modrm();
    // }

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

fn emit_modrm(buf: &mut Buffer, mod_: u8, reg: u8, rm: u8) {
    assert!(mod_ >= 0 && mod_ <= 7);
    assert!(reg >= 0 && reg <= 7);
    assert!(rm >= 0 && rm <= 3);

    buf.emit_u8(mod_ << 5 | reg << 2 | rm);
}
