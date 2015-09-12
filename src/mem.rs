use libc::*;
use std::ptr;
use std::mem;

pub struct CodeMemory {
    // size of memory area
    size: u32,

    // addr of full memory area
    ptr: *mut u8,
}

impl CodeMemory {
    pub fn new(buffer: &[u8]) -> CodeMemory {
        assert!(buffer.len() > 0);
        let size = align(buffer.len() as u32, 4096);

        unsafe {
            let ptr = mmap(0 as *mut c_void, size as u64, PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_ANON, -1, 0) as *mut u8;

            if ptr as *const c_void == MAP_FAILED {
                panic!("mmap failed");
            }

            ptr::copy_nonoverlapping(buffer.as_ptr(), ptr, buffer.len());

            CodeMemory {
                size: size,
                ptr: ptr
            }
        }
    }

    pub fn ptr(&self) -> *const u8 {
        self.ptr
    }

    pub fn func(&self) -> extern "C" fn() -> i32 {
        unsafe {
            mem::transmute(self.ptr)
        }
    }
}

pub fn align(value: u32, align: u32) -> u32 {
    ((value + align - 1) / align) * align
}

impl Drop for CodeMemory {
    fn drop(&mut self) {
        unsafe {
            munmap(self.ptr as *mut c_void, self.size as u64);
        }
    }
}
