use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::{Address, Collector};
use gc::root::{get_rootset, IndirectObj};
use gc::swiper::Region;
use mem;
use object::Obj;
use os::{self, ProtType};
use timer::{in_ms, Timer};

pub struct CopyCollector {
    total: Region,
    separator: Address,

    top: AtomicUsize,
    end: AtomicUsize,
}

impl CopyCollector {
    pub fn new(args: &Args) -> CopyCollector {
        let alignment = 2 * (os::page_size() as usize);
        let heap_size = mem::align_usize(args.heap_size(), alignment);
        let ptr = os::mmap(heap_size, os::Writable);

        if ptr.is_null() {
            panic!("could not allocate semi space of size {} bytes", heap_size);
        }

        let heap_start = Address::from_ptr(ptr);
        let heap_end = heap_start.offset(heap_size);

        let separator = heap_start.offset(heap_size / 2);

        CopyCollector {
            total: Region::new(heap_start, heap_end),
            separator: separator,
            top: AtomicUsize::new(heap_start.to_usize()),
            end: AtomicUsize::new(separator.to_usize()),
        }
    }
}

impl Collector for CopyCollector {
    fn alloc_obj(&self, ctxt: &SemContext, size: usize) -> *const u8 {
        if ctxt.args.flag_gc_stress {
            self.collect(ctxt);
        }

        let mut ptr = self.alloc(size);

        if ptr.is_null() {
            self.collect(ctxt);
            ptr = self.alloc(size);
        }

        ptr
    }

    fn alloc_array(
        &self,
        _ctxt: &SemContext,
        _elements: usize,
        _element_size: usize,
        _is_ref: bool,
    ) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, ctxt: &SemContext) {
        let rootset = get_rootset(ctxt);
        self.collect_from(ctxt, &rootset);
    }
}

impl Drop for CopyCollector {
    fn drop(&mut self) {
        os::munmap(self.total.start.to_ptr(), self.total.size());
    }
}

impl CopyCollector {
    fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.top.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new > self.end.load(Ordering::Relaxed) {
                return ptr::null();
            }

            let res = self.top.compare_exchange_weak(
                old,
                new,
                Ordering::SeqCst,
                Ordering::Relaxed,
            );

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old as *const u8
    }

    fn collect_from(&self, ctxt: &SemContext, rootset: &[IndirectObj]) {
        let mut timer = Timer::new(ctxt.args.flag_gc_events);

        // enable writing into to-space again (for debug builds)
        if cfg!(debug_assertions) {
            let to_space = self.to_space();
            os::mprotect(
                to_space.start.to_ptr(),
                to_space.size(),
                ProtType::Writable,
            );
        }

        // empty to-space
        let to_space = self.to_space();
        let from_space = self.from_space();

        let mut top = to_space.start;
        let mut scan = top;

        for root in rootset {
            let root_ptr = root.get();

            if from_space.contains(Address::from_ptr(root_ptr)) {
                root.set(self.copy(root_ptr, &mut top));
            }
        }

        while scan < top {
            let object: &mut Obj = unsafe { &mut *scan.to_mut_ptr() };

            object.visit_reference_fields(|field| {
                let field_ptr = field.get();

                if from_space.contains(Address::from_ptr(field_ptr)) {
                    field.set(self.copy(field_ptr, &mut top));
                }
            });

            scan = scan.offset(object.size());
        }

        // disable access in current from-space
        // makes sure that no pointer into from-space is left (in debug-builds)
        if cfg!(debug_assertions) {
            os::mprotect(
                from_space.start.to_ptr(),
                from_space.size(),
                ProtType::None,
            );
        }

        self.top.store(top.to_usize(), Ordering::Relaxed);
        self.end.store(to_space.end.to_usize(), Ordering::Relaxed);

        timer.stop_with(|dur| if ctxt.args.flag_gc_events {
            println!("Copy GC: collect garbage ({} ms)", in_ms(dur));
        });
    }

    fn copy(&self, obj: *mut Obj, top: &mut Address) -> *mut Obj {
        let obj = unsafe { &mut *obj };
        let addr = get_forwarding_address(obj);

        if is_forwarding_address(addr) {
            unmark_forwarding_address(addr).to_mut_ptr()

        } else {
            let addr = *top;
            let obj_size = obj.size();

            copy_object(obj, addr, obj_size);
            *top = top.offset(obj_size);

            set_forwarding_address(obj, addr);

            addr.to_mut_ptr()
        }
    }

    pub fn from_space(&self) -> Region {
        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            Region::new(self.total.start, self.separator)
        } else {
            Region::new(self.separator, self.total.end)
        }
    }

    pub fn to_space(&self) -> Region {
        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            Region::new(self.separator, self.total.end)
        } else {
            Region::new(self.total.start, self.separator)
        }
    }
}

fn copy_object(obj: &Obj, addr: Address, size: usize) {
    unsafe {
        ptr::copy_nonoverlapping(
            obj as *const Obj as *const u8,
            addr.to_mut_ptr::<u8>(),
            size,
        );
    }
}

pub fn is_forwarding_address(obj: Address) -> bool {
    obj.to_usize() & 1 == 1
}

pub fn mark_forwarding_address(obj: Address) -> Address {
    (obj.to_usize() | 1).into()
}

pub fn unmark_forwarding_address(obj: Address) -> Address {
    (obj.to_usize() & !1).into()
}

pub fn get_forwarding_address(obj: &Obj) -> Address {
    unsafe { *(obj as *const Obj as *const usize) }.into()
}

pub fn set_forwarding_address(obj: &mut Obj, addr: Address) {
    unsafe {
        *(obj as *mut Obj as *mut usize) = mark_forwarding_address(addr).to_usize();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mark_forwarding_address() {
        assert_eq!(Address::from(9), mark_forwarding_address(8.into()));
        assert_eq!(Address::from(13), mark_forwarding_address(12.into()));
    }

    #[test]
    fn test_unmark_forwarding_address() {
        assert_eq!(Address::from(8), unmark_forwarding_address(9.into()));
        assert_eq!(Address::from(12), unmark_forwarding_address(13.into()));
    }

    #[test]
    fn test_is_forwarding_address() {
        assert_eq!(false, is_forwarding_address(8.into()));
        assert_eq!(true, is_forwarding_address(9.into()));
        assert_eq!(false, is_forwarding_address(12.into()));
        assert_eq!(true, is_forwarding_address(13.into()));
    }
}
