use std::ptr;

use crate::gc::Address;
use crate::mem;
use crate::os::page_size;

#[cfg(target_family = "unix")]
pub fn reserve(size: usize) -> Address {
    debug_assert!(mem::is_page_aligned(size));

    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            size,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE,
            -1,
            0,
        ) as *mut libc::c_void
    };

    if ptr == libc::MAP_FAILED {
        panic!("reserving memory with mmap() failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_family = "windows")]
pub fn reserve(size: usize) -> Address {
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{MEM_RESERVE, PAGE_NOACCESS};

    let ptr = unsafe { VirtualAlloc(ptr::null_mut(), size, MEM_RESERVE, PAGE_NOACCESS) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }

    Address::from_ptr(ptr)
}

pub fn reserve_align(size: usize, align: usize) -> Address {
    debug_assert!(mem::is_page_aligned(size));
    debug_assert!(mem::is_page_aligned(align));

    let align_minus_page = align - page_size();

    let unaligned = reserve(size + align_minus_page);
    let aligned: Address = mem::align_usize(unaligned.to_usize(), align).into();

    let gap_start = aligned.offset_from(unaligned);
    let gap_end = align_minus_page - gap_start;

    if gap_start > 0 {
        uncommit(unaligned, gap_start);
    }

    if gap_end > 0 {
        uncommit(aligned.offset(size), gap_end);
    }

    aligned
}

#[cfg(target_family = "unix")]
pub fn commit(size: usize, executable: bool) -> Address {
    debug_assert!(mem::is_page_aligned(size));

    let mut prot = libc::PROT_READ | libc::PROT_WRITE;

    if executable {
        prot |= libc::PROT_EXEC;
    }

    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            size,
            prot,
            libc::MAP_PRIVATE | libc::MAP_ANON,
            -1,
            0,
        )
    };

    if ptr == libc::MAP_FAILED {
        panic!("committing memory with mmap() failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_family = "windows")]
pub fn commit(size: usize, executable: bool) -> Address {
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{MEM_COMMIT, MEM_RESERVE, PAGE_EXECUTE_READWRITE, PAGE_READWRITE};

    let prot = if executable {
        PAGE_EXECUTE_READWRITE
    } else {
        PAGE_READWRITE
    };

    let ptr = unsafe { VirtualAlloc(ptr::null_mut(), size, MEM_COMMIT | MEM_RESERVE, prot) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_family = "unix")]
pub fn commit_at(ptr: Address, size: usize, executable: bool) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let mut prot = libc::PROT_READ | libc::PROT_WRITE;

    if executable {
        prot |= libc::PROT_EXEC;
    }

    let val = unsafe {
        libc::mmap(
            ptr.to_mut_ptr(),
            size,
            prot,
            libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_FIXED,
            -1,
            0,
        )
    };

    if val == libc::MAP_FAILED {
        panic!("committing memory with mmap() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn commit_at(ptr: Address, size: usize, executable: bool) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{MEM_COMMIT, PAGE_EXECUTE_READWRITE, PAGE_READWRITE};

    let prot = if executable {
        PAGE_EXECUTE_READWRITE
    } else {
        PAGE_READWRITE
    };

    let result = unsafe { VirtualAlloc(ptr.to_mut_ptr(), size, MEM_COMMIT, prot) };

    if result != ptr.to_mut_ptr() {
        panic!("VirtualAlloc failed");
    }
}

#[cfg(target_family = "unix")]
pub fn uncommit(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let val = unsafe {
        libc::mmap(
            ptr.to_mut_ptr(),
            size,
            libc::PROT_NONE,
            libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_NORESERVE,
            -1,
            0,
        )
    };

    if val == libc::MAP_FAILED {
        panic!("uncommitting memory with mmap() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn uncommit(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualFree;
    use winapi::um::winnt::MEM_DECOMMIT;

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), size, MEM_DECOMMIT) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

#[cfg(target_family = "unix")]
pub fn discard(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let res = unsafe { libc::madvise(ptr.to_mut_ptr(), size, libc::MADV_DONTNEED) };

    if res != 0 {
        panic!("discarding memory with madvise() failed");
    }

    let res = unsafe { libc::mprotect(ptr.to_mut_ptr(), size, libc::PROT_NONE) };

    if res != 0 {
        panic!("discarding memory with mprotect() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn discard(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualFree;
    use winapi::um::winnt::MEM_DECOMMIT;

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), size, MEM_DECOMMIT) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

#[cfg(target_family = "unix")]
pub fn protect(start: Address, size: usize, access: Access) {
    debug_assert!(start.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    if access.is_none() {
        discard(start, size);
        return;
    }

    let protection = match access {
        Access::None => unreachable!(),
        Access::Read => libc::PROT_READ,
        Access::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
        Access::ReadExecutable => libc::PROT_READ | libc::PROT_EXEC,
        Access::ReadWriteExecutable => libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
    };

    let res = unsafe { libc::mprotect(start.to_mut_ptr(), size, protection) };

    if res != 0 {
        panic!("mprotect() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn protect(start: Address, size: usize, access: Access) {
    debug_assert!(start.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{
        MEM_COMMIT, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_READONLY, PAGE_READWRITE,
    };

    if access.is_none() {
        discard(start, size);
        return;
    }

    let protection = match access {
        Access::None => unreachable!(),
        Access::Read => PAGE_READONLY,
        Access::ReadWrite => PAGE_READWRITE,
        Access::ReadExecutable => PAGE_EXECUTE_READ,
        Access::ReadWriteExecutable => PAGE_EXECUTE_READWRITE,
    };

    let ptr = unsafe { VirtualAlloc(start.to_mut_ptr(), size, MEM_COMMIT, protection) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }
}

pub enum Access {
    None,
    Read,
    ReadWrite,
    ReadExecutable,
    ReadWriteExecutable,
}

impl Access {
    fn is_none(&self) -> bool {
        match self {
            Access::None => true,
            _ => false,
        }
    }
}
