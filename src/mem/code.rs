use libc::*;

use mem;
use os;

pub struct CodeMemory {
    // size of memory area
    size: u64,

    // addr of full memory area
    ptr: *mut c_void,
}

impl CodeMemory {
    pub fn new(size: u32) -> CodeMemory {
        let size = mem::align(size, os::page_size()) as u64;
        let ptr = os::mmap(size);

        CodeMemory {
            size: size,
            ptr: ptr,
        }
    }

    pub fn ptr(&self) -> *mut c_void {
        self.ptr
    }
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        os::munmap(self.ptr, self.size);
    }
}
