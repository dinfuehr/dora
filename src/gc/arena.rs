use std::ptr;

use gc::Address;
use mem;

pub fn reserve(size: usize) -> Address {
    use libc;

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

pub fn commit(ptr: Address, size: usize, executable: bool) {
    debug_assert!(mem::is_page_aligned(ptr.to_usize()));
    debug_assert!(mem::is_page_aligned(size));

    use libc;

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

pub fn uncommit(ptr: Address, size: usize) {
    use libc;

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

pub fn forget(ptr: Address, size: usize) {
    use libc;

    let res = unsafe {
        libc::madvise(ptr.to_mut_ptr(), size, libc::MADV_DONTNEED)
    };

    if res != 0 {
        panic!("forgetting memory with madvise() failed");
    }
}