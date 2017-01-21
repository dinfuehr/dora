use std::ptr;

use ctxt::Context;
use mem;
use os;

pub struct SemiSpace {
    start: *const u8,
    end: *const u8,

    next: *const u8,
}

impl SemiSpace {
    pub fn new(size: usize) -> SemiSpace {
        let size = mem::align_usize(size, os::page_size() as usize);
        let ptr = os::mmap(size, os::Writable);

        if ptr.is_null() {
            panic!("could not allocate semi space of size {} bytes", size);
        }

        SemiSpace {
            start: ptr,
            end: unsafe { ptr.offset(size as isize) },
            next: ptr,
        }
    }

    pub fn allocate(&mut self, size: usize) -> *const u8 {
        if self.end as usize - self.next as usize > size {
            let next = unsafe { self.next.offset(size as isize) };
            let addr = self.next;
            self.next = next;

            addr

        } else {
            ptr::null()
        }
    }

    pub fn reset(&mut self) {
        self.next = self.start;
    }
}

impl Drop for SemiSpace {
    fn drop(&mut self) {
        let size = self.end as usize - self.start as usize;
        os::munmap(self.start, size);
    }
}

pub fn swap_spaces(s1: &mut SemiSpace, s2: &mut SemiSpace) {
    let s = s1.start;
    let e = s1.end;
    let n = s1.next;

    s1.start = s2.start;
    s1.end = s2.end;
    s1.next = s2.next;

    s2.start = s;
    s2.end = e;
    s2.next = n;
}

pub fn minor_collect(_: &Context, from_space: &mut SemiSpace, to_space: &mut SemiSpace) {
    swap_spaces(from_space, to_space);
}
