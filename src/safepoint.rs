use libc;
use std::ptr;
use std::sync::atomic::{fence, Ordering};
use std::sync::Arc;

use cpu::fp_from_execstate;
use ctxt::get_vm;
use execstate::ExecState;
use gc::Address;
use os;
use threads::{DoraThread, ThreadState, THREAD};

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

pub fn enter(es: &ExecState) {
    let vm = get_vm();

    THREAD.with(|thread| {
        let thread = thread.borrow();
        let old_state = thread.state.load(Ordering::Relaxed);

        thread.saved_pc.store(es.pc, Ordering::Relaxed);
        thread
            .saved_fp
            .store(fp_from_execstate(es), Ordering::Relaxed);
        thread
            .state
            .store(ThreadState::Blocked as usize, Ordering::SeqCst);

        // During the stop-the-world-pause, the thread that initiates the pause
        // holds this lock. When the thread gives up the lock, execution can continue.
        let _mtx = vm.threads.threads.lock();

        // Restore old state.
        thread.state.store(old_state, Ordering::Relaxed);
    });
}

pub fn stop_the_world<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    let vm = get_vm();

    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();

    fence(Ordering::SeqCst);

    vm.polling_page.arm();
    pause_threads(&*threads);

    let ret = f();

    vm.polling_page.unarm();

    ret
}

fn pause_threads(_threads: &[Arc<DoraThread>]) {
    unimplemented!();
}
