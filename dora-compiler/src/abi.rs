use std::mem::offset_of;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize};

pub const CODE_ALIGNMENT: usize = 16;

#[repr(C)]
pub struct DoraToNativeInfo {
    pub last: *const DoraToNativeInfo,
    pub fp: usize,
    pub pc: usize,
}

impl DoraToNativeInfo {
    pub fn last_offset() -> i32 {
        offset_of!(DoraToNativeInfo, last) as i32
    }

    pub fn fp_offset() -> i32 {
        offset_of!(DoraToNativeInfo, fp) as i32
    }

    pub fn pc_offset() -> i32 {
        offset_of!(DoraToNativeInfo, pc) as i32
    }
}

#[repr(C)]
struct ThreadLocalDataLayout {
    tlab_top: AtomicUsize,
    tlab_end: AtomicUsize,
    stack_limit: AtomicUsize,
    dtn: AtomicUsize,
    managed_thread_handle: AtomicUsize,
    concurrent_marking: AtomicBool,
    state: AtomicU8,
    meta_space_start: usize,
}

pub fn thread_local_dtn_offset() -> i32 {
    offset_of!(ThreadLocalDataLayout, dtn) as i32
}
