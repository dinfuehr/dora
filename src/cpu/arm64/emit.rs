use baseline::buffer::Buffer;
use cpu::arm64::reg::*;
use cpu::arm64::asm::*;
use cpu::Reg;

pub fn load_int_const(buf: &mut Buffer, dest: Reg, imm: i32) {
    let register_size = 32;
    let imm = imm as i64 as u64;

    if fits_movz(imm, register_size) {
        buf.emit_u32(movz(0, dest, imm as u32, 0));

    } else if fits_movn(imm, register_size) {
        buf.emit_u32(movn(0, dest, imm as u32, 0));

    } else {
        unimplemented!();
    }
}

pub fn load_true(buf: &mut Buffer, dest: Reg) {
    buf.emit_u32(movz(0, dest, 1, 0));
}

pub fn load_false(buf: &mut Buffer, dest: Reg) {
    buf.emit_u32(movz(0, dest, 0, 0));
}