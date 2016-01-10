use libc::*;

use std::ptr;

use dseg::DSeg;
use mem::{CodeMemory, Ptr};

pub struct JitFct {
    code: CodeMemory,

    // pointer to beginning of function
    fct_start: Ptr,

    // machine code length in bytes
    fct_len: usize,
}

impl JitFct {
    pub fn new(dseg: &DSeg, buffer: &[u8]) -> JitFct {
        let size = dseg.size() as usize + buffer.len();

        let code = CodeMemory::new(size);
        let ptr = code.ptr();

        dseg.finish(ptr.raw_mut_ptr());

        let fct_start;

        unsafe {
            fct_start = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct_start.as_u8_mut_ptr(), buffer.len());
        }

        JitFct {
            code: code,
            fct_start: fct_start,
            fct_len: buffer.len(),
        }
    }

    pub fn fct_ptr(&self) -> Ptr {
        self.fct_start
    }

    pub fn fct_len(&self) -> usize {
        self.fct_len
    }
}
