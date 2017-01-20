pub use self::ProtType::*;

use libc;

use std::ptr;

#[cfg(target_family = "unix")]
pub fn page_size() -> u32 {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val == -1 {
        panic!("could not get page size.");
    }

    val as u32
}

#[cfg(target_family = "windows")]
pub fn page_size() -> u32 {
    use kernel32::GetSystemInfo;
    use winapi::sysinfoapi::SYSTEM_INFO;
    use std::mem;

    unsafe {
        let mut system_info: SYSTEM_INFO = mem::uninitialized();
        GetSystemInfo(&mut system_info);

        system_info.dwPageSize
    }
}

#[derive(PartialEq, Eq)]
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
        libc::mmap(ptr::null_mut(),
                   size,
                   libc::PROT_READ | libc::PROT_WRITE | prot_exec,
                   libc::MAP_PRIVATE | libc::MAP_ANON,
                   -1,
                   0) as *mut libc::c_void
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
