use cpu::*;
use cpu::asm::*;
use ctxt::{Context, FctId};
use execstate::ExecState;
use masm::MacroAssembler;

use mem::ptr::Ptr;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TrapId(u32);

impl TrapId {
    fn int(self) -> u32 {
        self.0
    }
}

pub const COMPILER: TrapId = TrapId(7);
pub const DIV0: TrapId = TrapId(9);
pub const ASSERT: TrapId = TrapId(10);
pub const INDEX_OUT_OF_BOUNDS: TrapId = TrapId(11);
pub const NIL: TrapId = TrapId(13);
pub const THROW: TrapId = TrapId(14);
pub const CAST: TrapId = TrapId(15);
pub const UNEXPECTED: TrapId = TrapId(16);

// emit stub instruction
pub fn emit(buf: &mut MacroAssembler, trap: TrapId) {
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

    if v1 == 0x25148b4c {
        Some(TrapId(v2))
    } else {
        None
    }
}

pub fn patch_fct_call(es: &mut ExecState, fct_ptr: Ptr) {
    // get return address from top of stack
    let mut ra : isize = unsafe { *(es.sp as *const isize) };

    // return address is now after `call *%rax` (needs 2 bytes),
    // we want to be before it
    ra -= 2;

    // return address is now after `movq (%rip, disp), %rax`, we also
    // want to be before it to execute it again
    ra -= 7;

    // get address of function pointer
    let disp_addr : *const i32 = (ra + 3) as *const i32;
    let disp : isize = unsafe { *disp_addr } as isize;

    let fct_addr : *mut usize = (ra + 7 + disp) as *mut usize;

    // write function pointer
    unsafe { *fct_addr = fct_ptr.raw() as usize; }

    // execute fct call again
    es.pc = fct_ptr.raw() as usize;
}

pub fn patch_vtable_call(ctxt: &Context, es: &mut ExecState, fid: FctId, fct_ptr: Ptr) {
    let fct = ctxt.fct_by_id(fid);
    let vtable_index = fct.vtable_index.unwrap();
    let cls_id = fct.owner_class.unwrap();

    let cls = ctxt.cls_by_id(cls_id);
    let vtable = cls.vtable.as_ref().unwrap();

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr.raw() as usize;

    // execute fct call again
    es.pc = fct_ptr.raw() as usize;
}
