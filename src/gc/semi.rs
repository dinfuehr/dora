use std::ptr;

use mem;
use os;

pub struct SemiSpace {
    start: *const u8,
    end: *const u8,
    next: *const u8,
}

impl SemiSpace {
    fn new(size: usize) -> SemiSpace {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, os::Writable);

        if ptr.is_null() {
            panic!("could not allocate semi space of size {}", size);
        }

        SemiSpace {
            start: ptr,
            end: unsafe { ptr.offset(size as isize) },
            next: ptr,
        }
    }

    fn allocate(&mut self, size: usize) -> *const u8 {
        if self.end as usize - self.next as usize > size {
            let next = unsafe { self.next.offset(size as isize) };
            let addr = self.next;
            self.next = next;

            addr

        } else {
            ptr::null()
        }
    }
}

impl Drop for SemiSpace {
    fn drop(&mut self) {
        let size = self.end as usize - self.start as usize;
        os::munmap(self.start, size);
    }
}