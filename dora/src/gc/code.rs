use parking_lot::Mutex;

use crate::gc::{Address, Region, K, M};
use crate::mem;
use crate::os::{self, MemoryPermission};

const TOTAL_SIZE: usize = 1 * M;
const CHUNK_SIZE: usize = 8 * K;
const CODE_ALIGNMENT: usize = 16;

/// Non-contiguous space of memory. Used for permanent space
/// and code space.
pub struct CodeSpace {
    total: Region,
    mutex: Mutex<AllocData>,
    chunk_size: usize,
}

struct AllocData {
    top: Address,
    limit: Address,
}

impl CodeSpace {
    pub fn new() -> CodeSpace {
        let reservation = os::reserve_align(TOTAL_SIZE, 0, true);
        let space_start = reservation.start;
        let space_end = space_start.offset(TOTAL_SIZE);

        let alloc_data = AllocData {
            top: space_start,
            limit: space_start,
        };

        CodeSpace {
            total: Region::new(space_start, space_end),
            mutex: Mutex::new(alloc_data),
            chunk_size: mem::page_align(CHUNK_SIZE),
        }
    }

    pub fn alloc(&self, size: usize) -> Address {
        debug_assert!(size > 0);

        let mut data = self.mutex.lock();
        let aligned_size = mem::align_usize(size, CODE_ALIGNMENT);

        if data.top.offset(aligned_size) > data.limit {
            let size = mem::align_usize(
                aligned_size - data.limit.offset_from(data.top),
                self.chunk_size,
            );
            let new_limit = data.limit.offset(size);

            if new_limit > self.total.end {
                panic!("OOM in code space");
            }

            let permissions = if cfg!(target_os = "macos") && cfg!(target_arch = "aarch64") {
                MemoryPermission::ReadExecute
            } else {
                MemoryPermission::ReadWriteExecute
            };

            os::protect(data.limit, size, permissions);
            data.limit = new_limit;
        }

        debug_assert!(data.top.offset(aligned_size) <= data.limit);
        let object_address = data.top;
        data.top = data.top.offset(aligned_size);
        object_address
    }
}

impl Drop for CodeSpace {
    fn drop(&mut self) {
        os::free(self.total.start, self.total.size());
    }
}
