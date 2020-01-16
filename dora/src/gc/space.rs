use parking_lot::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::{Address, Region};
use crate::mem;
use crate::os;

/// Configuration for a space.
/// This makes it possible to use `Space` both for the
/// code space and the permanent space.
pub struct SpaceConfig {
    pub executable: bool,
    pub chunk: usize,
    pub limit: usize,
    pub align: usize,
}

fn adapt_to_page_size(config: SpaceConfig) -> SpaceConfig {
    SpaceConfig {
        executable: config.executable,
        chunk: mem::page_align(config.chunk),
        limit: mem::page_align(config.limit),
        align: config.align,
    }
}

/// Non-contiguous space of memory. Used for permanent space
/// and code space.
pub struct Space {
    name: &'static str,
    config: SpaceConfig,
    total: Region,

    top: AtomicUsize,
    end: AtomicUsize,

    allocate: Mutex<()>,
}

impl Space {
    /// initializes `Space` and reserves the maximum size.
    pub fn new(config: SpaceConfig, name: &'static str) -> Space {
        let config = adapt_to_page_size(config);

        let space_start = os::reserve(config.limit);
        let space_end = space_start.offset(config.limit);

        os::commit_at(space_start, config.chunk, config.executable);
        let end = space_start.offset(config.chunk);

        Space {
            name,
            config,
            total: Region::new(space_start, space_end),

            top: AtomicUsize::new(space_start.to_usize()),
            end: AtomicUsize::new(end.to_usize()),

            allocate: Mutex::new(()),
        }
    }

    /// allocate memory in this space. This first tries to allocate space
    /// in the current chunk. If this fails a new chunk is allocated.
    /// Doesn't use a freelist right now so memory at the end of a chunk
    /// is probably lost.
    pub fn alloc(&self, size: usize) -> Address {
        let size = mem::align_usize(size, self.config.align);

        loop {
            let ptr = self.raw_alloc(size);
            if !ptr.is_null() {
                return ptr;
            }

            if !self.extend(size) {
                return Address::null();
            }
        }
    }

    fn raw_alloc(&self, size: usize) -> Address {
        let mut old = self.top.load(Ordering::Relaxed);
        let mut new;

        loop {
            new = old + size;

            if new > self.end.load(Ordering::Relaxed) {
                return Address::null();
            }

            let res = self
                .top
                .compare_exchange_weak(old, new, Ordering::SeqCst, Ordering::Relaxed);

            match res {
                Ok(_) => break,
                Err(x) => old = x,
            }
        }

        old.into()
    }

    fn extend(&self, size: usize) -> bool {
        let _lock = self.allocate.lock();

        let top = self.top.load(Ordering::Relaxed);
        let end = self.end.load(Ordering::Relaxed);

        if top + size <= end {
            return true;
        }

        let size = size - (end - top);
        let size = mem::align_usize(size, self.config.chunk);

        let new_end = end + size;

        if new_end <= self.total.end.to_usize() {
            os::commit_at(end.into(), size, self.config.executable);
            self.end.store(new_end, Ordering::SeqCst);

            true
        } else {
            false
        }
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn used_region(&self) -> Region {
        let start = self.total.start;
        let end = self.top.load(Ordering::Relaxed).into();

        Region::new(start, end)
    }
}
