use std::convert::From;

use baseline::map::CodeData;
use ctxt::SemContext;
use object::Obj;
use exception::DoraToNativeInfo;

pub fn get_rootset(ctxt: &SemContext) -> Vec<IndirectObj> {
    let mut rootset = Vec::new();

    determine_rootset_from_stack(&mut rootset, ctxt);
    determine_rootset_from_globals(&mut rootset, ctxt);
    determine_rootset_from_handles(&mut rootset, ctxt);

    rootset
}

fn determine_rootset_from_handles(rootset: &mut Vec<IndirectObj>, ctxt: &SemContext) {
    for rooted in ctxt.handles.iter() {
        rootset.push((rooted.raw() as usize).into());
    }
}

fn determine_rootset_from_globals(rootset: &mut Vec<IndirectObj>, ctxt: &SemContext) {
    for glob in ctxt.globals.iter() {
        let glob = glob.borrow();

        if !glob.ty.reference_type() {
            continue;
        }

        rootset.push((glob.address_value as usize).into());
    }
}

fn determine_rootset_from_stack(rootset: &mut Vec<IndirectObj>, ctxt: &SemContext) {
    assert!(!ctxt.dtn.borrow().is_null());

    let mut dtn = *ctxt.dtn.borrow();

    while !dtn.is_null() {
        dtn = from_dora_to_native_info(rootset, ctxt, dtn);
    }
}

fn from_dora_to_native_info(
    rootset: &mut Vec<IndirectObj>,
    ctxt: &SemContext,
    dtn: *const DoraToNativeInfo,
) -> *const DoraToNativeInfo {
    let dtn = unsafe { &*dtn };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        if !determine_rootset(rootset, ctxt, fp, pc) {
            break;
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }

    dtn.last
}

fn determine_rootset(
    rootset: &mut Vec<IndirectObj>,
    ctxt: &SemContext,
    fp: usize,
    pc: usize,
) -> bool {
    let code_map = ctxt.code_map.lock().unwrap();
    let data = code_map.get(pc as *const u8);

    if data.is_none() {
        return false;
    }

    if let CodeData::Fct(fct_id) = data.unwrap() {
        let jit_fct = ctxt.jit_fcts[fct_id].borrow();

        let offset = pc - (jit_fct.fct_ptr() as usize);
        let gcpoint = jit_fct
            .gcpoint_for_offset(offset as i32)
            .expect("no gcpoint");

        for &offset in &gcpoint.offsets {
            let addr = (fp as isize + offset as isize) as usize;
            rootset.push(addr.into());
        }

        true
    } else {
        false
    }
}

#[derive(Copy, Clone)]
pub struct IndirectObj(*mut *mut Obj);

impl IndirectObj {
    pub fn get(self) -> *mut Obj {
        unsafe { *self.0 }
    }

    pub fn set(self, obj: *mut Obj) {
        unsafe {
            *self.0 = obj;
        }
    }
}

impl From<usize> for IndirectObj {
    fn from(ptr: usize) -> IndirectObj {
        IndirectObj(ptr as *mut *mut Obj)
    }
}
