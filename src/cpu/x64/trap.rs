use cpu::*;
use cpu::instr::*;

use jit::buffer::Buffer;

pub static COMPILER: u32 = 7;
pub static DIV0: u32 = 8;

// emit stub instruction
pub fn emit(buf: &mut Buffer, id: u32) {
    let dest = R10;

    // mov r10, [trap::COMPILER]
    emit_rex(buf, 1, dest.msb(), 0, 0);
    emit_op(buf, 0x8b);
    emit_modrm(buf, 0, dest.and7(), 0b100);
    emit_sib(buf, 0, 0b100, 0b101);
    emit_u32(buf, id);
}
