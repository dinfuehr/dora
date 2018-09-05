use std::sync::atomic::{AtomicUsize, Ordering};

use gc::swiper::Region;
use gc::Address;
use os::{self, ProtType};

pub struct YoungGen {
    // bounds of from- & to-space
    pub total: Region,

    // size of both from- or to-space.
    // Not combined, use total.size() for that.
    size: usize,

    // address that separates from & to-space
    separator: Address,

    // address of next free memory
    free: AtomicUsize,

    // end of free memory (either separator or total.end)
    end: AtomicUsize,

    // separates survived from newly allocated objects
    // needed to decide whether to promote object into old space
    age_marker: AtomicUsize,
}

impl YoungGen {
    pub fn new(young_start: Address, young_end: Address) -> YoungGen {
        let half_size = young_end.offset_from(young_start) / 2;
        let half_address = young_start.offset(half_size);

        YoungGen {
            total: Region::new(young_start, young_end),
            size: half_size,
            separator: half_address,
            age_marker: AtomicUsize::new(young_start.to_usize()),
            free: AtomicUsize::new(young_start.to_usize()),
            end: AtomicUsize::new(half_address.to_usize()),
        }
    }

    pub fn used_region(&self) -> Region {
        Region::new(
            self.start_address(),
            self.free.load(Ordering::Relaxed).into(),
        )
    }

    fn start_address(&self) -> Address {
        if self.end.load(Ordering::Relaxed) == self.separator.to_usize() {
            self.total.start
        } else {
            self.separator
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

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn valid_top(&self, addr: Address) -> bool {
        self.total.valid_top(addr)
    }

    pub fn should_be_promoted(&self, addr: Address) -> bool {
        debug_assert!(self.contains(addr));

        addr.to_usize() < self.age_marker.load(Ordering::Relaxed)
    }

    pub fn alloc(&self, size: usize) -> Address {
        let mut old = self.free.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new > self.end.load(Ordering::Relaxed) {
                return Address::null();
            }

            let res =
                self.free
                    .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old.into()
    }

    pub fn swap_spaces(&self, free: Address) {
        self.end
            .store(self.to_space().end.to_usize(), Ordering::Relaxed);

        self.age_marker.store(free.to_usize(), Ordering::Relaxed);
        self.free.store(free.to_usize(), Ordering::Relaxed);
    }

    pub fn unprotect_to_space(&self) {
        // make memory writable again, so that we
        // can copy objects to the to-space.
        // Since this has some overhead, do it only in debug builds.

        if cfg!(debug_assertions) {
            let to_space = self.to_space();

            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::Writable,
            );
        }
    }

    pub fn protect_to_space(&self) {
        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) {
            let to_space = self.to_space();
            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::None,
            );
        }
    }
}
