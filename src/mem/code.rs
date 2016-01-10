use libc::*;

use mem;
use mem::Ptr;
use os;

pub struct CodeMemory {
    // size of memory area
    size: usize,

    // addr of full memory area
    ptr: Ptr,
}

impl CodeMemory {
    pub fn new(size: usize) -> CodeMemory {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, os::Executable);

        CodeMemory {
            size: size,
            ptr: ptr,
        }
    }

    pub fn ptr(&self) -> Ptr {
        self.ptr
    }
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        os::munmap(self.ptr, self.size);
    }
}
