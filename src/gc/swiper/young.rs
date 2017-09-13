use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;

use gc::Address;
use gc::swiper::Region;

pub struct YoungGen {
    total: Region,
    from: SemiSpace,
    to: SemiSpace,
}

impl YoungGen {
    pub fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = (young_end.to_usize() - young_start.to_usize()) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            from: SemiSpace::new(young_start, half_address),
            to: SemiSpace::new(half_address, young_end),
        }
    }

    pub fn alloc(&self, size: usize) -> *const u8 {
        let mut old = self.from.next.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new >= self.from.end.to_usize() {
                return ptr::null();
            }

            let res =
                self.from.next
                    .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old as *const u8
    }
}

struct SemiSpace {
    start: Address,
    next: AtomicUsize,
    uncommitted: Address,
    end: Address,
}

impl SemiSpace {
    fn new(start: Address, end: Address) -> SemiSpace {
        SemiSpace {
            start: start,
            next: AtomicUsize::new(start.to_usize()),
            uncommitted: start,
            end: end,
        }
    }

    fn empty() -> SemiSpace {
        SemiSpace {
            start: Address::null(),
            next: AtomicUsize::new(0),
            uncommitted: Address::null(),
            end: Address::null(),
        }
    }

    fn committed_size(&self) -> usize {
        self.uncommitted.to_usize() - self.start.to_usize()
    }

    fn uncommitted_size(&self) -> usize {
        self.end.to_usize() - self.uncommitted.to_usize()
    }
}
