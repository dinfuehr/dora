use std::ptr;

use exception::DoraToNativeInfo;
use gc::{Address, Region};

pub struct ThreadLocalData {
    d2n: *const DoraToNativeInfo,
    tlab_top: Address,
    tlab_end: Address,
    concurrent_marking: bool,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData {
            d2n: ptr::null(),
            tlab_top: Address::null(),
            tlab_end: Address::null(),
            concurrent_marking: false,
        }
    }

    pub fn tlab_initialize(&mut self, start: Address, end: Address) {
        self.tlab_top = start;
        self.tlab_end = end;
    }

    pub fn tlab_rest(&self) -> usize {
        self.tlab_end.offset_from(self.tlab_top)
    }

    pub fn tlab_region(&self) -> Region {
        Region::new(self.tlab_top, self.tlab_end)
    }

    pub fn tlab_top_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_top) as i32
    }

    pub fn tlab_end_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_end) as i32
    }

    pub fn concurrent_marking_offset() -> i32 {
        offset_of!(ThreadLocalData, concurrent_marking) as i32
    }
}
