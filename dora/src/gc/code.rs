use parking_lot::Mutex;

use crate::gc::{Address, Region, K};
use crate::mem;
use crate::os::{self, MemoryPermission};
use crate::vm::{ManagedCodeHeader, CODE_ALIGNMENT};

const CHUNK_SIZE: usize = 128 * K;

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
    pub fn new(limit: usize) -> CodeSpace {
        let reservation = os::reserve_align(limit, 0, true);
        let space_start = reservation.start;
        let space_end = space_start.offset(limit);

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

            os::protect(data.limit, size, MemoryPermission::ReadWriteExecute);
            data.limit = new_limit;
        }

        debug_assert!(data.top.offset(aligned_size) <= data.limit);
        let object_address = data.top;
        data.top = data.top.offset(aligned_size);
        object_address
    }

    pub fn allocated_region(&self) -> Region {
        let start = self.total.start;
        let end = self.mutex.lock().top;
        Region::new(start, end)
    }

    pub fn drop_all_native_code_objects(&self) {
        os::jit_writable();

        let allocated_region = self.allocated_region();
        let mut current = allocated_region.start;

        while current < allocated_region.end {
            let code_header = current.to_mut_ptr::<ManagedCodeHeader>();
            let code_header = unsafe { &mut *code_header };
            code_header.drop_native_code_object();

            let object = current.to_mut_obj();
            current = current.offset(object.size())
        }

        assert_eq!(current, allocated_region.end);
        os::jit_executable();
    }
}

impl Drop for CodeSpace {
    fn drop(&mut self) {
        os::free(self.total.start, self.total.size());
    }
}
