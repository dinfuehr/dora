use std::ptr;
use libc;

use execstate::ExecState;
use os;

pub struct PollingPage {
    addr: *const u8,
}

impl PollingPage {
    pub fn new() -> PollingPage {
        PollingPage {
            addr: alloc_polling_page(),
        }
    }

    pub fn addr(&self) -> *const u8 {
        self.addr
    }

    pub fn arm(&self) {
        unsafe {
            let res = libc::mprotect(
                self.addr as *mut libc::c_void,
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
                self.addr as *mut libc::c_void,
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
        os::munmap(self.addr, os::page_size() as usize);
    }
}

fn alloc_polling_page() -> *const u8 {
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

    ptr as *const u8
}

pub fn enter(_: &ExecState) {
    println!("enter safepoint");

    unsafe { libc::_exit(189) }
}
