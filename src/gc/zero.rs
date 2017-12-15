use std::sync::atomic::{AtomicPtr, Ordering};
use std::ptr;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::{arena, Collector};


pub struct ZeroCollector {
    start: *const u8,
    end: *const u8,
    next: AtomicPtr<u8>,
}

impl ZeroCollector {
    pub fn new(args: &Args) -> ZeroCollector {
        let heap_size: usize = args.flag_heap_size.map(|s| *s).unwrap_or(32 * 1024 * 1024);

        let ptr = arena::reserve(heap_size).expect("could not reserve memory");
        arena::commit(ptr, heap_size).expect("could not commit memory");

        ZeroCollector {
            start: ptr,
            end: unsafe { ptr.offset(heap_size as isize) },
            next: AtomicPtr::new(ptr),
        }
    }
}

impl Collector for ZeroCollector {
    fn alloc_obj(&self, _: &SemContext, size: usize) -> *const u8 {
        let mut old = self.next.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = unsafe { old.offset(size as isize) };

            if new as *const u8 >= self.end {
                return ptr::null();
            }

            let res = self.next.compare_exchange_weak(
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

        old
    }

    fn alloc_array(&self, _ctxt: &SemContext, _elements: usize, _element_size: usize, _is_ref: bool) -> *const u8 {
        unimplemented!()
    }

    fn collect(&self, _: &SemContext) {
        // do nothing
    }
}
