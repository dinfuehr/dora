use std::cell::RefCell;
use std::ptr;

use exception::DoraToNativeInfo;

thread_local! {
    pub static THREAD: RefCell<ThreadLocalData> = RefCell::new(ThreadLocalData::new());
}

pub struct ThreadLocalData {
    d2n: *const DoraToNativeInfo,
}

impl ThreadLocalData {
    pub fn new() -> ThreadLocalData {
        ThreadLocalData { d2n: ptr::null() }
    }
}
