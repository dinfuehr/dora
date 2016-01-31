use std::ptr;
use libc::*;

use mem;
use mem::Ptr;
use os;

pub struct CodeMemory {
    // size of memory area
    size: usize,

    // addr of full memory area
    ptr_start: Ptr,

    // end of full memory area
    ptr_end: Ptr
}

impl CodeMemory {
    pub fn new(size: usize) -> CodeMemory {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, os::Executable);

        CodeMemory {
            size: size,
            ptr_start: ptr,
            ptr_end: ptr.offset(size as isize)
        }
    }

    pub fn from_buffer(buffer: &[u8]) -> CodeMemory {
        let code = CodeMemory::new(buffer.len());

        unsafe {
            ptr::copy_nonoverlapping(buffer.as_ptr(),
                code.ptr_start.raw() as *mut u8, buffer.len());
        }

        code
    }

    pub fn ptr_start(&self) -> Ptr {
        self.ptr_start
    }

    pub fn ptr_end(&self) -> Ptr {
        self.ptr_end
    }
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        os::munmap(self.ptr_start, self.size);
    }
}
