use std::ptr;

use gc::Address;

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

pub fn commit(ptr: Address, size: usize) -> Result<(), ()> {
    use libc;

    let val = unsafe {
        libc::mmap(
            ptr.to_mut_ptr(),
            size,
            libc::PROT_READ | libc::PROT_WRITE,
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
