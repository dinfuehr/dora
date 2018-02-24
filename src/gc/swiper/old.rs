use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use gc::Address;
use gc::swiper::{CARD_SIZE, CARD_SIZE_BITS};
use gc::swiper::Region;

pub struct OldGen {
    pub total: Region,
    pub free: AtomicUsize,
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

        if (old >> CARD_SIZE_BITS) == (new >> CARD_SIZE_BITS) {
            if (old & (CARD_SIZE - 1)) == 0 {
                // card_crossing.set_first_object(old, 0);
            }
        } else {
            // card_crossing.set_first_object(card_start(new), new - card_start);
        }

        old as *const u8
    }

    fn card_from_address(&self, addr: Address) -> usize {
        debug_assert!(self.total.contains(addr));

        addr.offset_from(self.total.start) / CARD_SIZE
    }
}
