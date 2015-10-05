use libc::*;
use std::ptr;

use dseg::DSeg;

pub struct CodeMemory {
    // size of memory area
    size: u32,

    // addr of full memory area
    ptr: *mut c_void,

    // start addr of function
    fct: *const c_void
}

impl CodeMemory {
    pub fn new(dseg: &DSeg, buffer: &[u8]) -> CodeMemory {
        assert!(buffer.len() > 0);
        let size = dseg.size() + (buffer.len() as u32);
        let size = align(size, 4096);

        unsafe {
            let ptr = mmap(0 as *mut c_void, size as u64, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON, -1, 0) as *mut c_void;

            if ptr == MAP_FAILED {
                panic!("mmap failed");
            }

            dseg.finish(ptr as *mut c_void);

            let fct = ptr.offset(dseg.size() as isize);
            ptr::copy_nonoverlapping(buffer.as_ptr(), fct as *mut u8, buffer.len());

            CodeMemory {
                size: size,
                ptr: ptr,
                fct: fct,
            }
        }
    }

    pub fn fct(&self) -> *const c_void {
        self.fct
    }
}

pub fn align(value: u32, align: u32) -> u32 {
    ((value + align - 1) / align) * align
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        unsafe {
            munmap(self.ptr, self.size as u64);
        }
    }
}
