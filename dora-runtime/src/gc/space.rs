use parking_lot::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::{Address, Region, PAGE_SIZE};
use crate::mem;
use crate::os::{self, MemoryPermission, Reservation};
use crate::vm::VmFlags;

/// Configuration for a space.
/// This makes it possible to use `Space` both for the
/// code space and the permanent space.
pub struct SpaceConfig {
    pub executable: bool,
    pub chunk: usize,
    pub limit: usize,
    pub object_alignment: usize,
}

pub fn default_readonly_space_config(flags: &VmFlags) -> SpaceConfig {
    SpaceConfig {
        executable: false,
        chunk: PAGE_SIZE,
        limit: flags.readonly_size(),
        object_alignment: mem::ptr_width_usize(),
    }
}

fn adapt_to_page_size(config: SpaceConfig) -> SpaceConfig {
    SpaceConfig {
        executable: config.executable,
        chunk: mem::os_page_align_up(config.chunk),
        limit: mem::os_page_align_up(config.limit),
        object_alignment: config.object_alignment,
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
    reservation: Reservation,
}

impl Space {
    /// initializes `Space` and reserves the maximum size.
    pub fn new(config: SpaceConfig, name: &'static str) -> Space {
        let config = adapt_to_page_size(config);

        let reservation = os::reserve_align(config.limit, config.chunk, false);
        let space_start = reservation.start();
        let space_end = space_start.offset(config.limit);

        let permissions = if config.executable {
            MemoryPermission::ReadWriteExecute
        } else {
            MemoryPermission::ReadWrite
        };

        os::commit_at(space_start, config.chunk, permissions);
        let end = space_start.offset(config.chunk);

        Space {
            name,
            config,
            total: Region::new(space_start, space_end),

            top: AtomicUsize::new(space_start.to_usize()),
            end: AtomicUsize::new(end.to_usize()),

            allocate: Mutex::new(()),
            reservation,
        }
    }

    /// allocate memory in this space. This first tries to allocate space
    /// in the current chunk. If this fails a new chunk is allocated.
    /// Doesn't use a freelist right now so memory at the end of a chunk
    /// is probably lost.
    pub fn alloc(&self, size: usize) -> Address {
        let size = mem::align_usize_up(size, self.config.object_alignment);

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
        let size = mem::align_usize_up(size, self.config.chunk);

        let new_end = end + size;

        if new_end <= self.total.end.to_usize() {
            let permissions = if self.config.executable {
                MemoryPermission::ReadWriteExecute
            } else {
                MemoryPermission::ReadWrite
            };

            os::commit_at(end.into(), size, permissions);
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
