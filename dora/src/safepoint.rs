use std::sync::Arc;

use crate::stdlib;
use crate::threads::{DoraThread, ThreadState, THREAD};
use crate::vm::{get_vm, Trap, VM};

pub fn stop_the_world<F, R>(vm: &VM, operation: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    THREAD.with(|thread| thread.borrow().park(vm));

    let threads = vm.threads.threads.lock();
    if threads.len() == 1 {
        let ret = operation(&*threads);
        THREAD.with(|thread| thread.borrow().unpark(vm));
        return ret;
    }

    let safepoint_id = stop_threads(vm, &*threads);
    let ret = operation(&*threads);
    resume_threads(vm, &*threads, safepoint_id);
    THREAD.with(|thread| thread.borrow().unpark(vm));
    ret
}

fn current_thread_id() -> usize {
    THREAD.with(|thread| thread.borrow().id())
}

fn stop_threads(vm: &VM, threads: &[Arc<DoraThread>]) -> usize {
    let thread_self = THREAD.with(|thread| thread.borrow().clone());
    let safepoint_id = vm.threads.request_safepoint();

    vm.threads.barrier.guard(safepoint_id);

    for thread in threads.iter() {
        thread.tld.set_safepoint_requested();
    }

    while !all_threads_blocked(vm, &thread_self, threads, safepoint_id) {
        // do nothing
    }

    safepoint_id
}

fn all_threads_blocked(
    _vm: &VM,
    thread_self: &Arc<DoraThread>,
    threads: &[Arc<DoraThread>],
    safepoint_id: usize,
) -> bool {
    let mut all_blocked = true;

    for thread in threads {
        if Arc::ptr_eq(thread, thread_self) {
            assert!(thread.state().is_parked());
            continue;
        }

        if !thread.in_safepoint(safepoint_id) {
            all_blocked = false;
        }
    }

    all_blocked
}

fn resume_threads(vm: &VM, threads: &[Arc<DoraThread>], safepoint_id: usize) {
    for thread in threads.iter() {
        thread.tld.clear_safepoint_requested();
    }

    vm.threads.barrier.resume(safepoint_id);
    vm.threads.clear_safepoint_request();
}

pub extern "C" fn stack_overflow() {
    stdlib::trap(Trap::STACK_OVERFLOW.int());
}

pub extern "C" fn safepoint_slow() {
    let thread = THREAD.with(|thread| thread.borrow().clone());
    block(get_vm(), &thread);
}

pub fn block(vm: &VM, thread: &DoraThread) {
    let safepoint_id = vm.threads.safepoint_id();
    assert_ne!(safepoint_id, 0);
    let state = thread.state();

    match state {
        ThreadState::Running | ThreadState::Parked => {
            thread.block(safepoint_id);
        }

        ThreadState::Blocked => {
            panic!("illegal thread state: thread #{} {:?}", thread.id(), state);
        }

        ThreadState::ParkedSafepoint | ThreadState::RequestedSafepoint | ThreadState::Safepoint => {
            unreachable!()
        }
    };

    let _mtx = vm.threads.barrier.wait(safepoint_id);
    thread.unblock();
}
