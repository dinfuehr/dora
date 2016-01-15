use std::ptr;
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

    pub fn from_buffer(buffer: &[u8]) -> CodeMemory {
        let code = CodeMemory::new(buffer.len());

        unsafe {
            ptr::copy_nonoverlapping(buffer.as_ptr(),
                code.ptr.as_u8_mut_ptr(), buffer.len());
        }

        code
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
