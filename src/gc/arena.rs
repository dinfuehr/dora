use std::ptr;

use gc::Address;
use mem;

pub fn reserve(size: usize) -> Result<Address, ()> {
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
        Err(())
    } else {
        Ok(Address::from_ptr(ptr))
    }
}

pub fn commit(ptr: Address, size: usize, executable: bool) -> Result<(), ()> {
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

    if val != libc::MAP_FAILED {
        Ok(())
    } else {
        Err(())
    }
}

pub fn uncommit(ptr: Address, size: usize) -> Result<(), ()> {
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

    if val != libc::MAP_FAILED {
        Ok(())
    } else {
        Err(())
    }
}
