use std::fmt;
use std::convert::Into;
use std::ptr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub struct Ptr(*mut u8);

impl Ptr {
    pub fn new(ptr: *mut u8) -> Ptr {
        assert!(!ptr.is_null());

        Ptr(ptr)
    }

    pub fn null() -> Ptr {
        Ptr(ptr::null_mut())
    }

    pub fn offset(self, diff: isize) -> Ptr {
        Ptr(unsafe { self.0.offset(diff) })
    }

    pub fn raw(self) -> *mut u8 {
        self.0
    }
}

impl Into<Ptr> for usize {
    fn into(self) -> Ptr {
        Ptr::new(self as *mut u8)
    }
}

impl fmt::Debug for Ptr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Ptr({:x})", self.0 as usize)
    }
}
