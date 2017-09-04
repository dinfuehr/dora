use std::mem::swap;
use std::ptr;
use std::sync::Mutex;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::Collector;
use gc::root::{get_rootset, IndirectObj};
use mem;
use object::Obj;
use os;
use timer::{in_ms, Timer};

pub struct CopyCollector {
    spaces: Mutex<Spaces>,
}

struct Spaces {
    from_space: SemiSpace,
    to_space: SemiSpace,
}

impl Spaces {
    fn new(size: usize) -> Spaces {
        Spaces {
            from_space: SemiSpace::new(size),
            to_space: SemiSpace::new(size),
        }
    }
}

impl CopyCollector {
    pub fn new(args: &Args) -> CopyCollector {
        let heap_size = args.flag_heap_size.map(|s| *s).unwrap_or(32 * 1024 * 1024) / 2;

        CopyCollector {
            spaces: Mutex::new(Spaces::new(heap_size)),
        }
    }
}

impl Collector for CopyCollector {
    fn alloc(&self, ctxt: &SemContext, size: usize) -> *const u8 {
        let mut spaces = self.spaces.lock().unwrap();
        let spaces = &mut *spaces;

        if ctxt.args.flag_gc_stress {
            let rootset = get_rootset(ctxt);
            minor_collect(ctxt, &mut spaces.from_space, &mut spaces.to_space, rootset);
        }

        let mut ptr = spaces.to_space.allocate(size);

        if ptr.is_null() {
            let rootset = get_rootset(ctxt);
            minor_collect(ctxt, &mut spaces.from_space, &mut spaces.to_space, rootset);

            ptr = spaces.to_space.allocate(size);
        }

        ptr
    }

    fn collect(&self, ctxt: &SemContext) {
        let mut spaces = self.spaces.lock().unwrap();
        let spaces = &mut *spaces;

        let rootset = get_rootset(ctxt);
        minor_collect(ctxt, &mut spaces.from_space, &mut spaces.to_space, rootset);
    }
}

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
        debug_assert!(size % (mem::ptr_width() as usize) == 0);

        if self.end as usize - self.next as usize > size {
            let next = unsafe { self.next.offset(size as isize) };

            let addr = self.next;
            unsafe {
                ptr::write_bytes(addr as *mut u8, 0, size);
            }

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

    pub fn size(&self) -> usize {
        self.end as usize - self.start as usize
    }
}

impl Drop for SemiSpace {
    fn drop(&mut self) {
        let size = self.end as usize - self.start as usize;
        os::munmap(self.start, size);
    }
}

pub fn minor_collect(
    ctxt: &SemContext,
    from_space: &mut SemiSpace,
    to_space: &mut SemiSpace,
    rootset: Vec<IndirectObj>,
) {
    let mut timer = Timer::new(ctxt.args.flag_gc_events);
    swap(from_space, to_space);

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

    // memset from-space to garbage data for debug builds
    // makes sure that no pointer into from-space is left
    if cfg!(debug_assertions) {
        unsafe {
            ptr::write_bytes(from_space.start as *mut u8, 0xcc, from_space.size());
        }
    }

    timer.stop_with(|dur| {
        // self.collect_duration += dur;

        if ctxt.args.flag_gc_events {
            println!("GC minor: collect garbage ({} ms)", in_ms(dur));
        }
    });
}

pub fn copy(obj: *mut Obj, to_space: &mut SemiSpace) -> *mut Obj {
    let obj = unsafe { &mut *obj };
    let addr = get_forwarding_address(obj);

    if is_forwarding_address(addr) {
        unmark_forwarding_address(addr) as *mut Obj
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

pub fn mark_forwarding_address(obj: *const u8) -> *const u8 {
    ((obj as usize) | 1) as *const u8
}

pub fn unmark_forwarding_address(obj: *const u8) -> *const u8 {
    ((obj as usize) & !1) as *const u8
}

pub fn get_forwarding_address(obj: &Obj) -> *const u8 {
    unsafe { *(obj as *const Obj as *const *const u8) }
}

pub fn set_forwarding_address(obj: &mut Obj, addr: *const u8) {
    unsafe {
        *(obj as *mut Obj as *mut *const u8) = mark_forwarding_address(addr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mark_forwarding_address() {
        assert_eq!(9 as *const u8, mark_forwarding_address(8 as *const u8));
        assert_eq!(13 as *const u8, mark_forwarding_address(12 as *const u8));
    }

    #[test]
    fn test_unmark_forwarding_address() {
        assert_eq!(8 as *const u8, unmark_forwarding_address(9 as *const u8));
        assert_eq!(12 as *const u8, unmark_forwarding_address(13 as *const u8));
    }

    #[test]
    fn test_is_forwarding_address() {
        assert_eq!(false, is_forwarding_address(8 as *const u8));
        assert_eq!(true, is_forwarding_address(9 as *const u8));
        assert_eq!(false, is_forwarding_address(12 as *const u8));
        assert_eq!(true, is_forwarding_address(13 as *const u8));
    }
}
