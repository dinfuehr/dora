use std::sync::Arc;
use std::sync::atomic::Ordering;

use crate::runtime::{Runtime, RuntimeState, get_runtime};
use crate::threads::{DoraThread, current_thread, parked_scope};
use dora_compiler::ThreadState;

pub fn stop_the_world<F, R>(rt: &Runtime, operation: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    parked_scope(|| {
        let threads = rt.threads.threads.lock();

        if threads.len() == 1 {
            assert_eq!(
                current_thread() as *const _,
                threads.first().expect("missing thread").as_ref() as *const _
            );
            let ret = invoke_safepoint_operation(rt, &*threads, operation);
            return ret;
        }

        debug_assert!(
            threads
                .iter()
                .any(|t| t.as_ref() as *const _ == current_thread() as *const _)
        );

        stop_threads(rt, &*threads);
        let ret = invoke_safepoint_operation(rt, &*threads, operation);
        resume_threads(rt, &*threads);

        ret
    })
}

fn invoke_safepoint_operation<F, R>(rt: &Runtime, threads: &[Arc<DoraThread>], operation: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    let old_state = rt.set_state(RuntimeState::Safepoint);
    assert!(old_state.in_running());

    let result = operation(threads);

    let old_state = rt.set_state(RuntimeState::Running);
    assert!(old_state.in_safepoint());

    result
}

fn stop_threads(rt: &Runtime, threads: &[Arc<DoraThread>]) {
    rt.threads.barrier.arm();

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

    rt.threads.barrier.wait_until_threads_stopped(running);
}

fn resume_threads(rt: &Runtime, threads: &[Arc<DoraThread>]) {
    for thread in threads.iter() {
        let old_state: ThreadState = thread
            .tld
            .state
            .swap(ThreadState::Parked as u8, Ordering::SeqCst)
            .into();

        assert!(
            old_state == ThreadState::Safepoint
                || old_state == ThreadState::ParkedSafepointRequested
        );
    }

    rt.threads.barrier.disarm();
}

#[unsafe(export_name = "dora_native_safepoint_slow")]
pub extern "C" fn safepoint_slow() {
    let thread = current_thread();
    let rt = get_runtime();

    let state: ThreadState = thread
        .tld
        .state
        .swap(ThreadState::Safepoint as u8, Ordering::SeqCst)
        .into();
    assert_eq!(state, ThreadState::SafepointRequested);
    rt.threads.barrier.wait_in_safepoint();
    thread.unpark(rt);
}
