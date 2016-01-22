use cpu::*;
use cpu::instr::*;
use execstate::ExecState;

use jit::buffer::Buffer;

#[derive(Copy, Clone)]
pub struct TrapId(u32);

impl TrapId {
    fn int(self) -> u32 {
        self.0
    }
}

pub const COMPILER: TrapId = TrapId(7);
pub const DIV0: TrapId = TrapId(8);

// emit stub instruction
pub fn emit(buf: &mut Buffer, trap: TrapId) {
    let dest = R10;

    // mov r10, [trap::COMPILER]
    emit_rex(buf, 1, dest.msb(), 0, 0);
    emit_op(buf, 0x8b);
    emit_modrm(buf, 0, dest.and7(), 0b100);
    emit_sib(buf, 0, 0b100, 0b101);
    emit_u32(buf, trap.int());
}

pub fn read(es: &ExecState) -> Option<TrapId> {
    let v1;
    let v2;

    unsafe {
        let mut ptr: *const u32 = es.pc as *const u32;

        v1 = *ptr;
        ptr = ptr.offset(1);
        v2 = *ptr;
    }

    println!("v1 = {:x} v2 = {:x}", v1, v2);

    None
}
