use libc;
use parking_lot::{Condvar, Mutex};
use std::ptr;
use std::sync::atomic::{fence, Ordering};
use std::sync::Arc;

use crate::cpu::fp_from_execstate;
use crate::execstate::ExecState;
use crate::gc::Address;
use crate::os;
use crate::threads::{DoraThread, ThreadState, THREAD};
use crate::vm::{get_vm, VM};

pub struct PollingPage {
    addr: Address,
}

impl PollingPage {
    pub fn new() -> PollingPage {
        PollingPage {
            addr: alloc_polling_page(),
        }
    }

    pub fn addr(&self) -> Address {
        self.addr
    }

    pub fn arm(&self) {
        unsafe {
            let res = libc::mprotect(
                self.addr.to_mut_ptr(),
                os::page_size() as usize,
                libc::PROT_NONE,
            );

            if res != 0 {
                panic!("mprotect failed");
            }
        }
    }

    pub fn unarm(&self) {
        unsafe {
            let res = libc::mprotect(
                self.addr.to_mut_ptr(),
                os::page_size() as usize,
                libc::PROT_READ,
            );

            if res != 0 {
                panic!("mprotect failed");
            }
        }
    }
}

impl Drop for PollingPage {
    fn drop(&mut self) {
        os::munmap(self.addr.to_mut_ptr(), os::page_size() as usize);
    }
}

fn alloc_polling_page() -> Address {
    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            os::page_size() as usize,
            libc::PROT_READ,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            -1,
            0,
        ) as *mut libc::c_void
    };

    if ptr == libc::MAP_FAILED {
        panic!("mmap failed");
    }

    Address::from_ptr(ptr)
}

pub fn block(es: &ExecState) {
    let vm = get_vm();

    THREAD.with(|thread| {
        let thread = thread.borrow();
        let old_state = thread.state();

        match old_state {
            ThreadState::Dora => {
                block_dora(vm, &*thread, es);
            }

            ThreadState::Native => {
                // do nothing
            }

            _ => unreachable!(),
        }

        // During the stop-the-world-pause, the thread that initiates the pause
        // holds this lock. When the thread gives up the lock, execution can continue.
        let _mtx = vm.threads.threads.lock();

        // Restore old state.
        thread.set_state(old_state);
    });
}

fn block_dora(vm: &VM, thread: &Arc<DoraThread>, es: &ExecState) {
    thread.saved_pc.store(es.pc, Ordering::Relaxed);
    thread
        .saved_fp
        .store(fp_from_execstate(es), Ordering::Relaxed);
    thread
        .state
        .store(ThreadState::Blocked as usize, Ordering::SeqCst);

    let mut blocking = vm.safepoint.blocking.lock();

    assert!(*blocking > 0);
    *blocking -= 1;

    if *blocking == 0 {
        vm.safepoint.reached_zero.notify_all();
    }
}

pub struct Safepoint {
    blocking: Mutex<usize>,
    reached_zero: Condvar,
}

impl Safepoint {
    pub fn new() -> Safepoint {
        Safepoint {
            blocking: Mutex::new(0),
            reached_zero: Condvar::new(),
        }
    }
}

pub fn stop_the_world<F, R>(vm: &VM, f: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();

    fence(Ordering::SeqCst);

    {
        let mut blocking = vm.safepoint.blocking.lock();
        *blocking = threads.len();
    }

    vm.polling_page.arm();
    // pause_threads(vm, &*threads);

    let ret = f(&*threads);

    vm.polling_page.unarm();

    ret
}

fn pause_threads(vm: &VM, threads: &[Arc<DoraThread>]) {
    check_thread_states(vm, threads);
    wait_until_threads_reach_safepoints(vm);
}

fn check_thread_states(vm: &VM, threads: &[Arc<DoraThread>]) {
    for thread in threads {
        let state = thread.state();

        match state {
            ThreadState::Dora => {
                // let this thread continue until it reaches safepoint
            }

            ThreadState::Native => {
                // native threads can continue to run, we don't need to
                // wait for those threads to reach safepoints.
                let mut blocking = vm.safepoint.blocking.lock();
                assert!(*blocking > 0);
                *blocking -= 1;
            }

            ThreadState::Blocked => {
                // thread is already blocked via safepoint
            }

            ThreadState::Uninitialized => unreachable!(),
        }
    }
}

fn wait_until_threads_reach_safepoints(vm: &VM) {
    let mut blocking = vm.safepoint.blocking.lock();

    while *blocking > 0 {
        vm.safepoint.reached_zero.wait(&mut blocking);
    }
}
