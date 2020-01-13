use std::sync::Arc;

use crate::threads::DoraThread;
use crate::vm::VM;

pub fn stop_the_world<F, R>(vm: &VM, f: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();

    let ret = f(&*threads);

    ret
}
