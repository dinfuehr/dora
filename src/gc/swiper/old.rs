use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use gc::Address;
use gc::swiper::Region;

pub struct OldGen {
    total: Region,
    free: AtomicUsize,
}

impl OldGen {
    pub fn new(old_start: Address, old_end: Address) -> OldGen {
        OldGen {
            total: Region::new(old_start, old_end),
            free: AtomicUsize::new(old_start.to_usize()),
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.total.end.to_usize() {
                return ptr::null();
            }

            let res = self.free.compare_exchange_weak(
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
}
