use std::fmt;
use std::ptr;
use libc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Ptr(*mut libc::c_void);

impl Ptr {
    pub fn new(ptr: *mut libc::c_void) -> Ptr {
        assert!(!ptr.is_null());

        Ptr(ptr)
    }

    pub fn offset(self, diff: isize) -> Ptr {
        Ptr(unsafe { self.0.offset(diff) })
    }

    pub fn raw(self) -> *mut libc::c_void {
        self.0
    }
}

impl fmt::Debug for Ptr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ptr({:x})", self.0 as usize)
    }
}
