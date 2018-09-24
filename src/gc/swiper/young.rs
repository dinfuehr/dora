use std::sync::atomic::{AtomicUsize, Ordering};

use gc::arena;
use gc::swiper::Region;
use gc::Address;
use os::{self, ProtType};

pub struct YoungGen {
    // bounds of from- & to-space
    total: Region,

    // maximum semi-space size
    // Not combined, use total.size() for that.
    max_semi_size: usize,

    // comitted semi-space size
    committed_semi_size: usize,

    // address that separates from & to-space
    separator: Address,

    // address of next free memory
    free: AtomicUsize,

    // start and end of free memory in from-space
    committed_start: AtomicUsize,
    committed_end: AtomicUsize,

    // separates survived from newly allocated objects
    // needed to decide whether to promote object into old space
    age_marker: AtomicUsize,

    // set to true when unused memory regions should
    // be protected.
    protect: bool,
}

impl YoungGen {
    pub fn new(
        young_start: Address,
        young_end: Address,
        young_size: usize,
        protect: bool,
    ) -> YoungGen {
        let half_size = young_end.offset_from(young_start) / 2;
        let half_address = young_start.offset(half_size);

        let committed_semi_size = young_size / 2;

        let young = YoungGen {
            total: Region::new(young_start, young_end),
            max_semi_size: half_size,
            committed_semi_size: committed_semi_size,
            separator: half_address,
            age_marker: AtomicUsize::new(young_start.to_usize()),
            free: AtomicUsize::new(young_start.to_usize()),
            committed_start: AtomicUsize::new(young_start.to_usize()),
            committed_end: AtomicUsize::new(young_start.offset(committed_semi_size).to_usize()),
            protect: protect,
        };

        young.commit();

        young
    }

    fn commit(&self) {
        arena::commit(self.total.start, self.committed_semi_size, false);
        arena::commit(self.separator, self.committed_semi_size, false);
    }

    pub fn used_region(&self) -> Region {
        Region::new(
            self.start_address(),
            self.free.load(Ordering::Relaxed).into(),
        )
    }

    fn start_address(&self) -> Address {
        self.committed_start.load(Ordering::Relaxed).into()
    }

    pub fn from_space(&self) -> Region {
        self.start_address().region_start(self.committed_semi_size)
    }

    pub fn to_space(&self) -> Region {
        if self.committed_start.load(Ordering::Relaxed) == self.total.start.to_usize() {
            self.separator.region_start(self.committed_semi_size)
        } else {
            self.total.start.region_start(self.committed_semi_size)
        }
    }

    pub fn total(&self) -> Region {
        self.total.clone()
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

            if new > self.committed_end.load(Ordering::Relaxed) {
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

    // Switch from- & to-semi-space.
    pub fn swap_spaces(&self, free: Address) {
        let to_space = self.to_space();
        assert!(to_space.valid_top(free));
        self.committed_start
            .store(to_space.start.to_usize(), Ordering::Relaxed);
        self.committed_end
            .store(to_space.end.to_usize(), Ordering::Relaxed);

        self.age_marker.store(free.to_usize(), Ordering::Relaxed);
        self.free.store(free.to_usize(), Ordering::Relaxed);
    }

    // Free all objects in from-semi-space.
    pub fn free(&self) {
        let from_space = self.from_space();
        let start = from_space.start.to_usize();

        self.age_marker.store(start, Ordering::Relaxed);
        self.free.store(start, Ordering::Relaxed);
    }

    // Make to-semi-space writable.
    pub fn unprotect_to_space(&self) {
        // make memory writable again, so that we
        // can copy objects to the to-space.
        // Since this has some overhead, do it only in debug builds.

        if cfg!(debug_assertions) || self.protect {
            let to_space = self.to_space();

            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::Writable,
            );
        }
    }

    // Make to-semi-space inaccessible.
    pub fn protect_to_space(&self) {
        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) || self.protect {
            let to_space = self.to_space();
            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::None,
            );
        }
    }
}
