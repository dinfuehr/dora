use std::ptr;

use ctxt::Context;
use gc::root::IndirectObj;
use mem;
use object::Obj;
use os;
use timer::{in_ms, Timer};

pub struct SemiSpace {
    start: *const u8,
    end: *const u8,

    scan: *const u8,
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
            scan: ptr,
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

    pub fn includes(&self, ptr: *const u8) -> bool {
        self.start <= ptr && ptr < self.end
    }

    pub fn reset(&mut self) {
        self.scan = self.start;
        self.next = self.start;
    }
}

impl Drop for SemiSpace {
    fn drop(&mut self) {
        let size = self.end as usize - self.start as usize;
        os::munmap(self.start, size);
    }
}

pub fn minor_collect(ctxt: &Context,
                     from_space: &mut SemiSpace,
                     to_space: &mut SemiSpace,
                     rootset: Vec<IndirectObj>) {
    let mut timer = Timer::new(ctxt.args.flag_gc_dump );
    swap_spaces(from_space, to_space);

    // empty to-space
    to_space.reset();

    for &root in &rootset {
        let root_ptr = root.get();

        if from_space.includes(root_ptr as *const u8) {
            root.set(copy(root_ptr, to_space));
        }
    }

    while to_space.scan < to_space.next {
        let object = unsafe { &mut *(to_space.scan as *mut Obj) };

        object.visit_reference_fields(|child| {
            let child_ptr = child.get();

            if from_space.includes(child_ptr as *const u8) {
                child.set(copy(child_ptr, to_space));
            }
        });

        to_space.scan = unsafe { to_space.scan.offset(object.size() as isize) };
    }

    timer.stop_with(|dur| {
        // self.collect_duration += dur;

        if ctxt.args.flag_gc_dump {
            println!("GC: minor collect ({} ms)", in_ms(dur));
        }
    });
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

pub fn copy(obj: *mut Obj, to_space: &mut SemiSpace) -> *mut Obj {
    let obj = unsafe { &mut *obj };
    let addr = get_forwarding_address(obj);

    if is_forwarding_address(addr) {
        forwarding_address(addr) as *mut Obj

    } else {
        let addr = to_space.next;
        let obj_size = obj.size();

        unsafe {
            ptr::copy_nonoverlapping(obj as *const Obj as *const u8, addr as *mut u8, obj_size);
            to_space.next = to_space.next.offset(obj_size as isize);
        }

        set_forwarding_address(obj, addr);

        addr as *mut Obj
    }
}

pub fn is_forwarding_address(obj: *const u8) -> bool {
    (obj as usize) & 1 == 1
}

pub fn forwarding_address(obj: *const u8) -> *const u8 {
    ((obj as usize) & !1) as *const u8
}

pub fn get_forwarding_address(obj: &Obj) -> *const u8 {
    unsafe { *(obj as *const Obj as *const *const u8) }
}

pub fn set_forwarding_address(obj: &mut Obj, addr: *const u8) {
    unsafe {
        *(obj as *mut Obj as *mut *const u8) = addr;
    }
}
