use libc::*;

use std::collections::HashMap;
use std::fmt;
use std::ptr;

use dseg::DSeg;
use mem::{CodeMemory, Ptr};

pub struct JitFct {
    code: CodeMemory,

    // pointer to beginning of function
    fct_start: Ptr,

    // machine code length in bytes
    fct_len: usize,

    safepoints: Safepoints,
}

impl JitFct {
    pub fn new(dseg: &DSeg, buffer: &[u8]) -> JitFct {
        let size = dseg.size() as usize + buffer.len();

        let code = CodeMemory::new(size);
        let ptr = code.ptr_start();

        dseg.finish(ptr.raw());

        let fct_start;

        unsafe {
            fct_start = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct_start.raw() as *mut u8, buffer.len());
        }

        JitFct {
            code: code,
            safepoints: Safepoints::new(),
            fct_start: fct_start,
            fct_len: buffer.len(),
        }
    }

    pub fn code(self) -> CodeMemory {
        self.code
    }

    pub fn ptr_start(&self) -> Ptr {
        self.code.ptr_start()
    }

    pub fn ptr_end(&self) -> Ptr {
        self.code.ptr_end()
    }

    pub fn fct_ptr(&self) -> Ptr {
        self.fct_start
    }

    pub fn fct_len(&self) -> usize {
        self.fct_len
    }
}

impl fmt::Debug for JitFct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "JitFct {{ start: {:?}, end: {:?} }}",
            self.ptr_start(), self.ptr_end())
    }
}

pub struct Safepoints {
    points: HashMap<i32, Safepoint>
}

impl Safepoints {
    pub fn new() -> Safepoints {
        Safepoints {
            points: HashMap::new()
        }
    }
}

pub struct Safepoint {
    offsets: Vec<i32>
}

impl Safepoint {
    pub fn new() -> Safepoint {
        Safepoint {
            offsets: Vec::new()
        }
    }
}
