use std::ptr;

use exception::DoraToNativeInfo;

pub struct ThreadLocalData {
    d2n: *const DoraToNativeInfo,
    tlab_top: usize,
    tlab_end: usize,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData {
            d2n: ptr::null(),
            tlab_top: 0,
            tlab_end: 0,
        }
    }

    pub fn tlab_top_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_top) as i32
    }

    pub fn tlab_end_offset() -> i32 {
        offset_of!(ThreadLocalData, tlab_end) as i32
    }
}
