use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::threads::{current_thread, parked_scope, DoraThread, ThreadState};
use crate::vm::{get_vm, VmState, VM};

pub fn stop_the_world<F, R>(vm: &VM, operation: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    parked_scope(|| {
        let threads = vm.threads.threads.lock();

        if threads.len() == 1 {
            assert_eq!(
                current_thread() as *const _,
                threads.first().expect("missing thread").as_ref() as *const _
            );
            let ret = invoke_safepoint_operation(vm, &*threads, operation);
            return ret;
        }

        debug_assert!(threads
            .iter()
            .any(|t| t.as_ref() as *const _ == current_thread() as *const _));

        stop_threads(vm, &*threads);
        let ret = invoke_safepoint_operation(vm, &*threads, operation);
        resume_threads(vm, &*threads);

        ret
    })
}

fn invoke_safepoint_operation<F, R>(vm: &VM, threads: &[Arc<DoraThread>], operation: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    let old_state = vm.set_state(VmState::Safepoint);
    assert!(old_state.in_running());

    let result = operation(threads);

    let old_state = vm.set_state(VmState::Running);
    assert!(old_state.in_safepoint());

    result
}

fn stop_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    vm.threads.barrier.arm();

    let mut running = 0;

    for thread in threads.iter() {
        let current_state = thread
            .tld
            .state
            .fetch_or(ThreadState::SafepointRequested as u8, Ordering::SeqCst);

        if current_state == ThreadState::Running as u8 {
            running += 1;
        } else {
            assert_eq!(current_state, ThreadState::Parked as u8);
        }
    }

    vm.threads.barrier.wait_until_threads_stopped(running);
}

fn resume_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads.iter() {
        let old_state: ThreadState = thread
            .tld
            .state
            .swap(ThreadState::Parked as u8, Ordering::SeqCst)
            .into();

        assert!(old_state == ThreadState::Safepoint || old_state == ThreadState::ParkedSafepoint);
    }

    vm.threads.barrier.disarm();
}

pub extern "C" fn safepoint_slow() {
    let thread = current_thread();
    let vm = get_vm();

    let state: ThreadState = thread
        .tld
        .state
        .swap(ThreadState::Safepoint as u8, Ordering::SeqCst)
        .into();
    assert_eq!(state, ThreadState::SafepointRequested);
    vm.threads.barrier.wait_in_safepoint();
    thread.unpark(vm);
}
