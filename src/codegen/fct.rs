use libc::*;
use std::ptr;

use dseg::DSeg;
use mem::CodeMemory;

pub struct JitFct {
    code: CodeMemory,

    // pointer to beginning of function
    fct_start: *mut c_void,
}

impl JitFct {
    pub fn new(dseg: &DSeg, buffer: &[u8]) -> JitFct {
        let size = dseg.size() as usize + buffer.len();

        let code = CodeMemory::new(size);
        let ptr = code.ptr();

        dseg.finish(ptr);

        let fct_start;

        unsafe {
            fct_start = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct_start as *mut u8, buffer.len());
        }

        JitFct {
            code: code,
            fct_start: fct_start,
        }
    }

    pub fn fct(&self) -> *mut c_void {
        self.fct_start
    }
}
