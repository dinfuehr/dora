use libc::*;

use mem;

pub struct CodeMemory {
    // size of memory area
    size: u32,

    // addr of full memory area
    ptr: *mut c_void,
}

impl CodeMemory {
    pub fn new(size: u32) -> CodeMemory {
        let size = mem::align(size, 4096);

        unsafe {
            let ptr = mmap(0 as *mut c_void, size as u64, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON, -1, 0) as *mut c_void;

            if ptr == MAP_FAILED {
                panic!("mmap failed");
            }

            CodeMemory {
                size: size,
                ptr: ptr,
            }
        }
    }

    pub fn ptr(&self) -> *mut c_void {
        self.ptr
    }
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        unsafe {
            munmap(self.ptr, self.size as u64);
        }
    }
}
