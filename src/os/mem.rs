pub use self::ProtType::*;

use libc;

use std::ptr;

static mut PAGE_SIZE: u32 = 0;
static mut PAGE_SIZE_BITS: u32 = 0;

pub fn init_page_size() {
    unsafe {
        PAGE_SIZE = determine_page_size();
        assert!((PAGE_SIZE & (PAGE_SIZE - 1)) == 0);

        PAGE_SIZE_BITS = log2(PAGE_SIZE);
    }
}

#[cfg(target_family = "unix")]
fn determine_page_size() -> u32 {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val <= 0 {
        panic!("could not determine page size.");
    }

    val as u32
}

#[cfg(target_family = "windows")]
pub fn determine_page_size() -> u32 {
    use kernel32::GetSystemInfo;
    use winapi::sysinfoapi::SYSTEM_INFO;
    use std::mem;

    unsafe {
        let mut system_info: SYSTEM_INFO = mem::uninitialized();
        GetSystemInfo(&mut system_info);

        system_info.dwPageSize
    }
}

/// determine log_2 of given value
fn log2(mut val: u32) -> u32 {
    let mut log = 0;

    if (val & 0xFFFF0000) != 0 {
        val >>= 16;
        log += 16;
    }
    if val >= 256 {
        val >>= 8;
        log += 8;
    }
    if val >= 16 {
        val >>= 4;
        log += 4;
    }
    if val >= 4 {
        val >>= 2;
        log += 2;
    }

    log + (val >> 1)
}

#[test]
fn test_log2() {
    for i in 0..32 {
        assert_eq!(i, log2(1 << i));
    }
}

pub fn page_size() -> u32 {
    unsafe { PAGE_SIZE }
}

pub fn page_size_bits() -> u32 {
    unsafe { PAGE_SIZE_BITS }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ProtType {
    Executable,
    Writable,
}

#[cfg(target_family = "unix")]
pub fn mmap(size: usize, exec: ProtType) -> *const u8 {
    let prot_exec = if exec == Executable {
        libc::PROT_EXEC
    } else {
        0
    };

    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            size,
            libc::PROT_READ | libc::PROT_WRITE | prot_exec,
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

#[cfg(target_family = "windows")]
pub fn mmap(size: usize, exec: ProtType) -> *const u8 {
    use kernel32::VirtualAlloc;
    use winapi::winnt::{MEM_COMMIT, MEM_RESERVE, PAGE_EXECUTE_READWRITE, PAGE_READWRITE};

    let prot = if exec == Executable {
        PAGE_EXECUTE_READWRITE
    } else {
        PAGE_READWRITE
    };

    let ptr = unsafe { VirtualAlloc(ptr::null_mut(), size as u64, MEM_COMMIT | MEM_RESERVE, prot) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }

    ptr as *const u8
}

#[cfg(target_family = "unix")]
pub fn munmap(ptr: *const u8, size: usize) {
    let res = unsafe { libc::munmap(ptr as *mut libc::c_void, size) };

    if res != 0 {
        panic!("munmap failed");
    }
}

#[cfg(target_family = "windows")]
pub fn munmap(ptr: *const u8, size: usize) {
    use kernel32::VirtualFree;
    use winapi::winnt::MEM_RELEASE;
    use winapi;

    let res = unsafe { VirtualFree(ptr as *mut winapi::c_void, 0, MEM_RELEASE) };

    if res == 0 {
        panic!("VirtualFree failed");
    }
}
