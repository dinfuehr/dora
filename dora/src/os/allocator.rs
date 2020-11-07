use std::ptr;

use crate::gc::Address;
use crate::mem;
use crate::os::page_size;

#[cfg(target_family = "unix")]
fn reserve(size: usize) -> Address {
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
fn reserve(size: usize) -> Address {
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{MEM_RESERVE, PAGE_NOACCESS};

    let ptr = unsafe { VirtualAlloc(ptr::null_mut(), size, MEM_RESERVE, PAGE_NOACCESS) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_family = "unix")]
pub fn free(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let result = unsafe { libc::munmap(ptr.to_mut_ptr(), size) };

    if result != 0 {
        panic!("munmap() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn free(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualFree;
    use winapi::um::winnt::MEM_RELEASE;

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), 0, MEM_RELEASE) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

pub struct Reservation {
    pub start: Address,

    pub unaligned_start: Address,
    pub unaligned_size: usize,
}

pub fn reserve_align(size: usize, align: usize) -> Reservation {
    debug_assert!(mem::is_page_aligned(size));
    debug_assert!(mem::is_page_aligned(align));

    let align = if align == 0 { page_size() } else { align };
    let unaligned_size = size + align - page_size();

    let unaligned_start = reserve(unaligned_size);
    let aligned_start: Address = mem::align_usize(unaligned_start.to_usize(), align).into();

    let gap_start = aligned_start.offset_from(unaligned_start);
    let gap_end = unaligned_size - size - gap_start;

    if gap_start > 0 {
        uncommit(unaligned_start, gap_start);
    }

    if gap_end > 0 {
        uncommit(aligned_start.offset(size), gap_end);
    }

    if cfg!(target_family = "unix") {
        Reservation {
            start: aligned_start,
            unaligned_start: aligned_start,
            unaligned_size: size,
        }
    } else if cfg!(target_family = "windows") {
        Reservation {
            start: aligned_start,
            unaligned_start,
            unaligned_size,
        }
    } else {
        unreachable!();
    }
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
pub fn commit_at(ptr: Address, size: usize, permissions: MemoryPermissions) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let protection = match permissions {
        MemoryPermissions::None => libc::PROT_NONE,
        MemoryPermissions::Read => libc::PROT_READ,
        MemoryPermissions::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
        MemoryPermissions::ReadExecute => libc::PROT_READ | libc::PROT_EXEC,
        MemoryPermissions::ReadWriteExecute => libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
    };

    let val = unsafe {
        libc::mmap(
            ptr.to_mut_ptr(),
            size,
            protection,
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
pub fn commit_at(ptr: Address, size: usize, permissions: MemoryPermissions) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{
        MEM_COMMIT, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_NOACCESS, PAGE_READONLY,
        PAGE_READWRITE,
    };

    let protection = match permissions {
        MemoryPermissions::None => PAGE_NOACCESS,
        MemoryPermissions::Read => PAGE_READONLY,
        MemoryPermissions::ReadWrite => PAGE_READWRITE,
        MemoryPermissions::ReadExecute => PAGE_EXECUTE_READ,
        MemoryPermissions::ReadWriteExecute => PAGE_EXECUTE_READWRITE,
    };

    let result = unsafe { VirtualAlloc(ptr.to_mut_ptr(), size, MEM_COMMIT, protection) };

    if result != ptr.to_mut_ptr() {
        panic!("VirtualAlloc failed");
    }
}

#[cfg(target_family = "unix")]
fn uncommit(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    let result = unsafe { libc::munmap(ptr.to_mut_ptr(), size) };

    if result != 0 {
        panic!("munmap() failed");
    }
}

#[cfg(target_family = "windows")]
fn uncommit(ptr: Address, size: usize) {
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
pub fn protect(start: Address, size: usize, permissions: MemoryPermissions) {
    debug_assert!(start.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    if permissions == MemoryPermissions::None {
        discard(start, size);
        return;
    }

    let protection = match permissions {
        MemoryPermissions::None => libc::PROT_NONE,
        MemoryPermissions::Read => libc::PROT_READ,
        MemoryPermissions::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
        MemoryPermissions::ReadExecute => libc::PROT_READ | libc::PROT_EXEC,
        MemoryPermissions::ReadWriteExecute => libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
    };

    let res = unsafe { libc::mprotect(start.to_mut_ptr(), size, protection) };

    if res != 0 {
        panic!("mprotect() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn protect(start: Address, size: usize, access: MemoryPermissions) {
    debug_assert!(start.is_page_aligned());
    debug_assert!(mem::is_page_aligned(size));

    use winapi::um::memoryapi::VirtualAlloc;
    use winapi::um::winnt::{
        MEM_COMMIT, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_READONLY, PAGE_READWRITE,
    };

    if access == MemoryPermissions::None {
        discard(start, size);
        return;
    }

    let protection = match access {
        MemoryPermissions::None => unreachable!(),
        MemoryPermissions::Read => PAGE_READONLY,
        MemoryPermissions::ReadWrite => PAGE_READWRITE,
        MemoryPermissions::ReadExecute => PAGE_EXECUTE_READ,
        MemoryPermissions::ReadWriteExecute => PAGE_EXECUTE_READWRITE,
    };

    let ptr = unsafe { VirtualAlloc(start.to_mut_ptr(), size, MEM_COMMIT, protection) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MemoryPermissions {
    None,
    Read,
    ReadWrite,
    ReadExecute,
    ReadWriteExecute,
}
