use std::ptr;

use mem::is_page_aligned;
use os;

pub struct Arena {
    pub start: *mut u8,
    pub size: usize,
    pub pages: Vec<u8>,
}

impl Arena {
    pub fn new(size: usize) -> Arena {
        debug_assert!(is_page_aligned(size));
        let pages = size >> os::page_size_bits();

        Arena {
            start: reserve(size),
            size: size,
            pages: Vec::with_capacity(0),
        }
    }

    pub fn alloc(&mut self, ptr: *const u8, size: usize) -> *const u8 {
        debug_assert!(is_page_aligned(size));

        unimplemented!()
    }

    pub fn free(&mut self, ptr: *const u8, size: usize) {
        debug_assert!(is_page_aligned(size));
        let ind = (ptr as usize - self.start as usize) >> os::page_size_bits();
        let pages = size >> os::page_size_bits();

        // mark pages free
        for el in &mut self.pages[ind..ind+pages] {
            *el = 0;
        }
    }
}

fn reserve(size: usize) -> *mut u8 {
    use libc;

    let ptr = unsafe {
        libc::mmap(ptr::null_mut(),
                   size,
                   libc::PROT_NONE,
                   libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_NORESERVE,
                   -1,
                   0) as *mut libc::c_void
    };

    if ptr == libc::MAP_FAILED {
        ptr::null_mut()
    } else {
        ptr as *mut u8
    }
}

fn commit(ptr: *const u8, size: usize) -> bool {
    use libc;

    unsafe {
        libc::mmap(ptr as *mut libc::c_void,
                   size,
                   libc::PROT_READ | libc::PROT_WRITE,
                   libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_FIXED,
                   -1,
                   0) != libc::MAP_FAILED
    }
}

fn uncommit(ptr: *const u8, size: usize) -> bool {
    use libc;

    unsafe {
        libc::mmap(ptr as *mut libc::c_void,
                   size,
                   libc::PROT_NONE,
                   libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_NORESERVE,
                   -1,
                   0) != libc::MAP_FAILED
    }
}