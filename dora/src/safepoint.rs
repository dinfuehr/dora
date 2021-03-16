use std::sync::atomic::Ordering;
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

    stop_threads(vm, &*threads);
    let ret = operation(&*threads);
    resume_threads(vm, &*threads);

    THREAD.with(|thread| thread.borrow().unpark(vm));

    ret
}

fn current_thread_id() -> usize {
    THREAD.with(|thread| thread.borrow().id())
}

fn stop_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    vm.threads.barrier.arm();

    for thread in threads.iter() {
        thread.tld.set_safepoint_requested();
    }

    let mut running = 0;

    for thread in threads.iter() {
        let mut current_state = thread.state_relaxed();

        loop {
            let next_state = match current_state {
                ThreadState::Running => ThreadState::RequestedSafepoint,
                ThreadState::Parked => ThreadState::ParkedSafepoint,
                ThreadState::Safepoint => {
                    running += 1;
                    break;
                }
                state => panic!("unexpected state {:?} when stopping threads", state),
            };

            match thread.atomic_state.compare_exchange(
                current_state as usize,
                next_state as usize,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => {
                    if current_state == ThreadState::Running {
                        running += 1;
                    }

                    break;
                }

                Err(state) => {
                    current_state = state.into();
                }
            }
        }
    }

    vm.threads.barrier.wait_until_threads_stopped(running);
}

fn resume_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads.iter() {
        thread.tld.clear_safepoint_requested();
    }

    for thread in threads.iter() {
        let old_state: ThreadState = thread
            .atomic_state
            .swap(ThreadState::Parked as usize, Ordering::SeqCst)
            .into();

        assert!(old_state == ThreadState::Safepoint || old_state == ThreadState::ParkedSafepoint);
    }

    vm.threads.barrier.disarm();
}

pub extern "C" fn stack_overflow() {
    stdlib::trap(Trap::STACK_OVERFLOW.int());
}

pub extern "C" fn safepoint_slow() {
    let thread = THREAD.with(|thread| thread.borrow().clone());
    let vm = get_vm();

    let state: ThreadState = thread
        .atomic_state
        .swap(ThreadState::Safepoint as usize, Ordering::SeqCst)
        .into();
    assert!(state == ThreadState::RequestedSafepoint || state == ThreadState::Running);
    vm.threads.barrier.wait_in_safepoint();
    thread.unpark(vm);
}
