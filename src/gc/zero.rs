use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use ctxt::SemContext;
use driver::cmd::Args;
use gc::Address;
use gc::{arena, Collector};


pub struct ZeroCollector {
    start: Address,
    end: Address,
    next: AtomicUsize,
}

impl ZeroCollector {
    pub fn new(args: &Args) -> ZeroCollector {
        let heap_size: usize = args.heap_size();

        let ptr = arena::reserve(heap_size).expect("could not reserve memory");
        arena::commit(ptr, heap_size, false).expect("could not commit memory");

        ZeroCollector {
            start: ptr,
            end: ptr.offset(heap_size),
            next: AtomicUsize::new(ptr.to_usize()),
        }
    }
}

impl Collector for ZeroCollector {
    fn alloc_obj(&self, _: &SemContext, size: usize) -> *const u8 {
        let mut old = self.next.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.end.to_usize() {
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

        old as *const u8
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

    fn collect(&self, _: &SemContext) {
        // do nothing
    }
}
