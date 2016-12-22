use std::ptr;

use mem;
use os;

pub struct CodeMemory {
    // size of memory area
    size: usize,

    // addr of full memory area
    ptr_start: *const u8,

    // end of full memory area
    ptr_end: *const u8,
}

impl CodeMemory {
    pub fn new(size: usize) -> CodeMemory {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, os::Executable);

        let ptr_end = unsafe { ptr.offset(size as isize) };

        CodeMemory {
            size: size,
            ptr_start: ptr,
            ptr_end: ptr_end,
        }
    }

    pub fn from_buffer(buffer: &[u8]) -> CodeMemory {
        let code = CodeMemory::new(buffer.len());

        unsafe {
            ptr::copy_nonoverlapping(buffer.as_ptr(), code.ptr_start() as *mut u8, buffer.len());
        }

        code
    }

    pub fn ptr_start(&self) -> *const u8 {
        self.ptr_start
    }

    pub fn ptr_end(&self) -> *const u8 {
        self.ptr_end
    }
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        os::munmap(self.ptr_start, self.size);
    }
}
