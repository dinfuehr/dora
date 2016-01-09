pub use self::ProtType::*;

use libc;

pub fn page_size() -> u32 {
    let val = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };

    if val == -1 { panic!("could not get page size."); }

    val as u32
}

#[derive(PartialEq, Eq)]
pub enum ProtType {
    Executable, NonExecutable
}

pub fn mmap(size: usize, exec: ProtType) -> *mut libc::c_void {
    let prot_exec = if exec == Executable {
        libc::PROT_EXEC
    } else {
        0
    };

    let ptr = unsafe {
        libc::mmap(0 as *mut libc::c_void, size,
            libc::PROT_READ | libc::PROT_WRITE | prot_exec,
            libc::MAP_PRIVATE | libc::MAP_ANON, -1, 0) as *mut libc::c_void
    };

    if ptr == libc::MAP_FAILED {
        panic!("mmap failed");
    }

    ptr
}

pub fn munmap(ptr: *mut libc::c_void, size: usize) {
    let res = unsafe {
        libc::munmap(ptr, size)
    };

    if res != 0 {
        panic!("munmap failed");
    }
}
