use std::sync::Arc;

use crate::gc::{fill_region, Address, Region, K};
use crate::threads::{current_thread, DoraThread};
use crate::vm::VM;

pub const MIN_TLAB_SIZE: usize = 8 * K;
pub const MAX_TLAB_SIZE: usize = 32 * K;
pub const MAX_TLAB_OBJECT_SIZE: usize = 8 * K;

pub fn initialize(tlab: Region) {
    current_thread().tld.tlab_initialize(tlab.start, tlab.end);
}

pub fn calculate_size() -> usize {
    MAX_TLAB_SIZE
}

pub fn allocate(size: usize) -> Option<Address> {
    assert!(size < MAX_TLAB_OBJECT_SIZE);

    let thread = current_thread();
    let tlab = thread.tld.tlab_region();

    if size <= tlab.size() {
        thread
            .tld
            .tlab_initialize(tlab.start.offset(size), tlab.end);
        Some(tlab.start)
    } else {
        None
    }
}

pub fn make_iterable_all(vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads {
        let tlab = thread.tld.tlab_region();
        fill_region(vm, tlab.start, tlab.end);

        let n = Address::null();
        thread.tld.tlab_initialize(n, n);
    }
}

pub fn make_iterable_current(vm: &VM) {
    let thread = current_thread();
    let tlab = thread.tld.tlab_region();

    fill_region(vm, tlab.start, tlab.end);

    let n = Address::null();
    thread.tld.tlab_initialize(n, n);
}
