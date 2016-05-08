use std::fmt;
use std::convert::Into;
use std::ptr;
use libc;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Ptr(*mut libc::c_void);

impl Ptr {
    pub fn new(ptr: *mut libc::c_void) -> Ptr {
        assert!(!ptr.is_null());

        Ptr(ptr)
    }

    pub fn null() -> Ptr {
        Ptr(ptr::null_mut())
    }

    pub fn offset(self, diff: isize) -> Ptr {
        Ptr(unsafe { self.0.offset(diff) })
    }

    pub fn raw(self) -> *mut libc::c_void {
        self.0
    }
}

impl Into<Ptr> for usize {
    fn into(self) -> Ptr {
        Ptr::new(self as *mut libc::c_void)
    }
}

impl fmt::Debug for Ptr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ptr({:x})", self.0 as usize)
    }
}
