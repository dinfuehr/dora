use std::ptr;

use crate::gc::Address;
use crate::mem;
use crate::os::page_size;

#[cfg(target_family = "unix")]
fn reserve(size: usize, jitting: bool) -> Address {
    debug_assert!(mem::is_os_page_aligned(size));

    let mut flags = libc::MAP_PRIVATE | libc::MAP_ANON;

    if jitting {
        flags |= map_jit_flag();
    }

    let ptr = unsafe {
        libc::mmap(ptr::null_mut(), size, libc::PROT_NONE, flags, -1, 0) as *mut libc::c_void
    };

    if ptr == libc::MAP_FAILED {
        panic!("reserving memory with mmap() failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_os = "macos")]
fn map_jit_flag() -> i32 {
    libc::MAP_JIT
}

#[cfg(not(target_os = "macos"))]
fn map_jit_flag() -> i32 {
    0
}

#[cfg(target_family = "windows")]
fn reserve(size: usize, _jitting: bool) -> Address {
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::VirtualAlloc;
    use windows_sys::Win32::System::Memory::{MEM_RESERVE, PAGE_NOACCESS};

    let ptr = unsafe { VirtualAlloc(ptr::null_mut(), size, MEM_RESERVE, PAGE_NOACCESS) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }

    Address::from_ptr(ptr)
}

#[cfg(target_family = "unix")]
pub fn free(ptr: Address, size: usize) {
    debug_assert!(ptr.is_os_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    let result = unsafe { libc::munmap(ptr.to_mut_ptr(), size) };

    if result != 0 {
        panic!("munmap() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn free(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{MEM_RELEASE, VirtualFree};

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), 0, MEM_RELEASE) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

pub struct Reservation {
    start: Address,
    size: usize,

    unaligned_start: Address,
    unaligned_size: usize,
}

impl Reservation {
    pub fn start(&self) -> Address {
        self.start
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

impl Drop for Reservation {
    fn drop(&mut self) {
        free(self.unaligned_start, self.unaligned_size);
    }
}

pub fn reserve_align(size: usize, align: usize, jitting: bool) -> Reservation {
    debug_assert!(mem::is_os_page_aligned(size));
    debug_assert!(mem::is_os_page_aligned(align));

    let align = if align == 0 { page_size() } else { align };
    let unaligned_size = size + align - page_size();

    let unaligned_start = reserve(unaligned_size, jitting);
    let aligned_start: Address = mem::align_usize_up(unaligned_start.to_usize(), align).into();

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
            size,
            unaligned_start: aligned_start,
            unaligned_size: size,
        }
    } else if cfg!(target_family = "windows") {
        Reservation {
            start: aligned_start,
            size,
            unaligned_start,
            unaligned_size,
        }
    } else {
        unreachable!();
    }
}

pub fn commit_align(size: usize, align: usize, jitting: bool) -> Reservation {
    debug_assert!(mem::is_os_page_aligned(size));
    debug_assert!(mem::is_os_page_aligned(align));

    let align = if align == 0 { page_size() } else { align };
    let unaligned_size = size + align - page_size();

    let unaligned_start = commit(unaligned_size, jitting);
    let aligned_start: Address = mem::align_usize_up(unaligned_start.to_usize(), align).into();

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
            size,
            unaligned_start: aligned_start,
            unaligned_size: size,
        }
    } else if cfg!(target_family = "windows") {
        Reservation {
            start: aligned_start,
            size,
            unaligned_start,
            unaligned_size,
        }
    } else {
        unreachable!();
    }
}

#[cfg(target_family = "unix")]
pub fn commit(size: usize, executable: bool) -> Address {
    debug_assert!(mem::is_os_page_aligned(size));

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
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{
        MEM_COMMIT, MEM_RESERVE, PAGE_EXECUTE_READWRITE, PAGE_READWRITE, VirtualAlloc,
    };

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
pub fn commit_at(ptr: Address, size: usize, permissions: MemoryPermission) {
    debug_assert!(ptr.is_os_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    let protection = match permissions {
        MemoryPermission::None => libc::PROT_NONE,
        MemoryPermission::Read => libc::PROT_READ,
        MemoryPermission::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
        MemoryPermission::ReadExecute => libc::PROT_READ | libc::PROT_EXEC,
        MemoryPermission::ReadWriteExecute => libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
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
pub fn commit_at(ptr: Address, size: usize, permissions: MemoryPermission) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{
        MEM_COMMIT, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_NOACCESS, PAGE_READONLY,
        PAGE_READWRITE, VirtualAlloc,
    };

    let protection = match permissions {
        MemoryPermission::None => PAGE_NOACCESS,
        MemoryPermission::Read => PAGE_READONLY,
        MemoryPermission::ReadWrite => PAGE_READWRITE,
        MemoryPermission::ReadExecute => PAGE_EXECUTE_READ,
        MemoryPermission::ReadWriteExecute => PAGE_EXECUTE_READWRITE,
    };

    let result = unsafe { VirtualAlloc(ptr.to_mut_ptr(), size, MEM_COMMIT, protection) };

    if result != ptr.to_mut_ptr() {
        panic!("VirtualAlloc failed");
    }
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
pub fn jit_writable() {
    unsafe extern "C" {
        fn pthread_jit_write_protect_np(value: libc::c_int);
    }

    unsafe {
        pthread_jit_write_protect_np(0);
    }
}

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
pub fn jit_writable() {
    // nothing
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
pub fn jit_executable() {
    unsafe extern "C" {
        fn pthread_jit_write_protect_np(value: libc::c_int);
    }

    unsafe {
        pthread_jit_write_protect_np(1);
    }
}

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
pub fn jit_executable() {
    // nothing
}

#[cfg(target_family = "unix")]
fn uncommit(ptr: Address, size: usize) {
    debug_assert!(ptr.is_os_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    let result = unsafe { libc::munmap(ptr.to_mut_ptr(), size) };

    if result != 0 {
        panic!("munmap() failed");
    }
}

#[cfg(target_family = "windows")]
fn uncommit(ptr: Address, size: usize) {
    debug_assert!(ptr.is_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{MEM_DECOMMIT, VirtualFree};

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), size, MEM_DECOMMIT) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

#[cfg(target_family = "unix")]
pub fn discard(ptr: Address, size: usize) {
    debug_assert!(ptr.is_os_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

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
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{MEM_DECOMMIT, VirtualFree};

    let result = unsafe { VirtualFree(ptr.to_mut_ptr(), size, MEM_DECOMMIT) };

    if result == 0 {
        panic!("VirtualFree failed");
    }
}

#[cfg(target_family = "unix")]
pub fn protect(start: Address, size: usize, permission: MemoryPermission) {
    debug_assert!(start.is_os_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    if permission == MemoryPermission::None {
        discard(start, size);
        return;
    }

    let protection = match permission {
        MemoryPermission::None => libc::PROT_NONE,
        MemoryPermission::Read => libc::PROT_READ,
        MemoryPermission::ReadWrite => libc::PROT_READ | libc::PROT_WRITE,
        MemoryPermission::ReadExecute => libc::PROT_READ | libc::PROT_EXEC,
        MemoryPermission::ReadWriteExecute => libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
    };

    let res = unsafe { libc::mprotect(start.to_mut_ptr(), size, protection) };

    if res != 0 {
        panic!("mprotect() failed");
    }
}

#[cfg(target_family = "windows")]
pub fn protect(start: Address, size: usize, access: MemoryPermission) {
    debug_assert!(start.is_page_aligned());
    debug_assert!(mem::is_os_page_aligned(size));

    use windows_sys::Win32::System::Memory::{
        MEM_COMMIT, PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_READONLY, PAGE_READWRITE,
        VirtualAlloc,
    };

    if access == MemoryPermission::None {
        discard(start, size);
        return;
    }

    let protection = match access {
        MemoryPermission::None => unreachable!(),
        MemoryPermission::Read => PAGE_READONLY,
        MemoryPermission::ReadWrite => PAGE_READWRITE,
        MemoryPermission::ReadExecute => PAGE_EXECUTE_READ,
        MemoryPermission::ReadWriteExecute => PAGE_EXECUTE_READWRITE,
    };

    let ptr = unsafe { VirtualAlloc(start.to_mut_ptr(), size, MEM_COMMIT, protection) };

    if ptr.is_null() {
        panic!("VirtualAlloc failed");
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MemoryPermission {
    None,
    Read,
    ReadWrite,
    ReadExecute,
    ReadWriteExecute,
}
