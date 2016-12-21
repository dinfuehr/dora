use ctxt::{Context, FctId};
use execstate::ExecState;

use mem::ptr::Ptr;
use os::signal::Trap;

pub fn read(es: &ExecState) -> Option<Trap> {
    let v1;
    let v2;

    unsafe {
        let mut ptr: *const u32 = es.pc as *const u32;

        v1 = *ptr;
        ptr = ptr.offset(1);
        v2 = *ptr;
    }

    if v1 == 0x25148b4c {
        Trap::from(v2)
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
