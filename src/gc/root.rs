use std::convert::From;

use baseline::map::CodeData;
use ctxt::{Context, FctKind};
use object::Obj;

pub fn get_rootset(ctxt: &Context) -> Vec<IndirectObj> {
    let mut rootset = Vec::new();

    let mut pc: usize;
    let mut fp: usize;

    assert!(!ctxt.sfi.borrow().is_null());

    {
        let sfi = unsafe { &**ctxt.sfi.borrow() };

        pc = sfi.ra;
        fp = sfi.fp;
    }

    while fp != 0 {
        if !determine_rootset(&mut rootset, ctxt, fp, pc) {
            break;
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }

    rootset
}

fn determine_rootset(rootset: &mut Vec<IndirectObj>, ctxt: &Context, fp: usize, pc: usize) -> bool {
    let code_map = ctxt.code_map.lock().unwrap();
    let data = code_map.get(pc as *const u8);

    if data.is_none() {
        return false;
    }

    if let CodeData::Fct(fct_id) = data.unwrap() {
        let fct = ctxt.fcts[fct_id].borrow();

        if let FctKind::Source(ref src) = fct.kind {
            let src = src.borrow();
            let jit_fct = src.jit_fct.read().unwrap();
            let jit_fct = jit_fct.as_ref().expect("no jit information");
            let offset = pc - (jit_fct.fct_ptr() as usize);
            let gcpoint = jit_fct.gcpoint_for_offset(offset as i32).expect("no gcpoint");

            for &offset in &gcpoint.offsets {
                let addr = (fp as isize + offset as isize) as usize;
                rootset.push(addr.into());
            }
        } else {
            panic!("should be FctKind::Source");
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
