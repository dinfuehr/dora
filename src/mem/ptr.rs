use std::ptr;
use libc;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct Ptr(*const libc::c_void);

impl Ptr {
    pub fn null() -> Ptr {
        Ptr(ptr::null())
    }

    pub fn new(ptr: *const libc::c_void) -> Ptr {
        Ptr(ptr)
    }

    pub fn offset(self, diff: isize) -> Ptr {
        Ptr(unsafe { self.0.offset(diff) })
    }

    pub fn is_null(self) -> bool {
        self.0.is_null()
    }

    pub fn raw_ptr(self) -> *const libc::c_void {
        self.0 as *const libc::c_void
    }

    pub fn raw_mut_ptr(self) -> *mut libc::c_void {
        self.0 as *mut libc::c_void
    }

    pub fn as_u8_ptr(self) -> *const u8 {
        self.0 as *const u8
    }

    pub fn as_u8_mut_ptr(self) -> *mut u8 {
        self.0 as *mut u8
    }

    pub fn as_u64(self) -> u64 {
        self.0 as u64
    }
}
