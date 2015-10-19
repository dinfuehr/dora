use libc::*;
use std::ptr;

use dseg::DSeg;
use mem::code::CodeMemory;

pub struct JitFct {
    code: CodeMemory,

    // pointer to beginning of function
    fct: *mut c_void,
}

impl JitFct {
    pub fn new(dseg: &DSeg, buffer: &[u8]) -> JitFct {
        let size = dseg.size() + (buffer.len() as u32);

        let code = CodeMemory::new(size);
        let ptr = code.ptr();

        dseg.finish(ptr);

        let fct;

        unsafe {
            fct = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct as *mut u8, buffer.len());
        }

        JitFct {
            code: code,
            fct: fct,
        }
    }

    pub fn fct(&self) -> *mut c_void {
        self.fct
    }
}
