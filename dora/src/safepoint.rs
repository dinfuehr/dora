use libc;
use std::ptr;
use std::sync::Arc;

use crate::gc::Address;
use crate::os;
use crate::threads::DoraThread;
use crate::vm::VM;

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

pub fn stop_the_world<F, R>(vm: &VM, f: F) -> R
where
    F: FnOnce(&[Arc<DoraThread>]) -> R,
{
    // lock threads from starting or exiting
    let threads = vm.threads.threads.lock();

    let ret = f(&*threads);

    ret
}
