use std::sync::Arc;

use crate::stdlib;
use crate::threads::{DoraThread, THREAD};
use crate::vm::{get_vm, stack_pointer, Trap, VM};

pub fn stop_the_world<F, R>(vm: &VM, f: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();
    stop_threads(vm, &*threads);
    let ret = f(&*threads);
    resume_threads(vm, &*threads);
    ret
}

fn stop_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    let thread_self = THREAD.with(|thread| thread.borrow().clone());
    let mut stopped = vm.threads.stopped.lock();

    let mut thread_count = 0;

    for thread in threads.iter() {
        if Arc::ptr_eq(&thread_self, thread) {
            continue;
        }
        thread.tld.arm_stack_guard();
        thread_count = thread_count + 1;
    }

    *stopped = thread_count;

    while *stopped > 0 {
        vm.threads.reached_zero.wait(&mut stopped);
    }
}

fn resume_threads(_vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads.iter() {
        thread.tld.unarm_stack_guard();
    }
}

pub extern "C" fn guard_check() {
    let thread = THREAD.with(|thread| thread.borrow().clone());
    let stack_overflow = thread.tld.real_stack_limit() > stack_pointer();

    if stack_overflow {
        stdlib::trap(Trap::STACK_OVERFLOW.int());
    } else {
        stop_thread(get_vm());
    }
}

fn stop_thread(vm: &VM) {
    {
        let mut stopped = vm.threads.stopped.lock();

        assert!(*stopped > 0);
        *stopped -= 1;

        if *stopped == 0 {
            vm.threads.reached_zero.notify_all();
        }
    }

    // During the stop-the-world-pause, the thread that initiates the pause
    // holds this lock. When the thread gives up the lock, execution can continue.
    let _mtx = vm.threads.threads.lock();
}
