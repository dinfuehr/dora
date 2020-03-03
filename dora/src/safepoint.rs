use std::sync::Arc;

use crate::stdlib;
use crate::threads::{DoraThread, THREAD};
use crate::vm::{get_vm, stack_pointer, Trap, VM};

pub fn stop_the_world<F, R>(vm: &VM, f: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    THREAD.with(|thread| thread.borrow().park(vm));
    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();
    stop_threads(vm, &*threads);
    let ret = f(&*threads);
    resume_threads(vm, &*threads);
    THREAD.with(|thread| thread.borrow().unpark(vm));
    ret
}

fn current_thread_id() -> usize {
    THREAD.with(|thread| thread.borrow().id())
}

fn stop_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    let thread_self = THREAD.with(|thread| thread.borrow().clone());
    let mut stopped = vm.threads.stopped.lock();

    vm.threads.barrier.guard();

    for thread in threads.iter() {
        thread.tld.arm_stack_guard();
    }

    let mut thread_count = 0;

    for thread in threads.iter() {
        if Arc::ptr_eq(&thread_self, thread) {
            continue;
        }

        if thread.block() {
            thread_count = thread_count + 1;
        } else {
            // thread is already in safepoint
        }
    }

    *stopped = thread_count;

    while *stopped > 0 {
        vm.threads.reached_zero.wait(&mut stopped);
    }
}

fn resume_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads.iter() {
        thread.tld.unarm_stack_guard();
    }

    let thread_self = THREAD.with(|thread| thread.borrow().clone());

    for thread in threads.iter() {
        if Arc::ptr_eq(&thread_self, thread) {
            continue;
        }

        thread.unblock();
    }

    vm.threads.barrier.resume();
}

pub extern "C" fn guard_check() {
    let thread = THREAD.with(|thread| thread.borrow().clone());
    let stack_overflow = thread.tld.real_stack_limit() > stack_pointer();

    if stack_overflow {
        stdlib::trap(Trap::STACK_OVERFLOW.int());
    } else {
        enter_safepoint_from_dora(get_vm());
    }
}

fn enter_safepoint_from_dora(vm: &VM) {
    decrement_stopped(vm);
    vm.threads.barrier.wait();
}

pub fn enter_safepoint_from_native_park(vm: &VM) {
    decrement_stopped(vm);
    vm.threads.barrier.wait();
}

pub fn enter_safepoint_from_native_unpark(vm: &VM) {
    vm.threads.barrier.wait();
}

fn decrement_stopped(vm: &VM) {
    let mut stopped = vm.threads.stopped.lock();

    assert!(*stopped > 0);
    *stopped -= 1;

    if *stopped == 0 {
        vm.threads.reached_zero.notify_all();
    }
}
