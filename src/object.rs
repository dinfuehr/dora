use std::ptr;

use gc::Gc;
use mem;
use mem::ptr::Ptr;

// String in Dora is immutable
// length + string content is stored in one contiguous memory
pub struct Str {
    ptr: Ptr
}

impl Str {
    pub fn len(&self) -> usize {
        unsafe {
            *(self.ptr.raw() as *const usize)
        }
    }

    pub fn ptr(&self) -> Ptr {
        self.ptr
    }

    pub fn set_len(&self, len: usize) {
        unsafe {
            *(self.ptr.raw() as *mut usize) = len;
        }
    }

    pub fn data(&self) -> *mut u8 {
        unsafe {
            self.ptr.raw().offset(mem::ptr_width() as isize) as *mut u8
        }
    }
}

impl Str {
    pub fn from_buffer(gc: &mut Gc, buf: &[u8]) -> Str {
        let size = mem::ptr_width() as usize + buf.len() + 1;
        let string = Str { ptr: gc.alloc(size) };

        unsafe {
            // write len of Str (excluding 0 at end)
            string.set_len(buf.len());

            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), string.data(), buf.len());

            // string should end with 0 for C compatibility
            *(string.data().offset(buf.len() as isize)) = 0;
        }

        string
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use gc::Gc;
    use mem;
    use libc;

    #[test]
    pub fn test_create_string() {
        let mut gc = Gc::new();
        let string = Str::from_buffer(&mut gc, "hello world!".as_bytes());

        unsafe {
            assert_eq!(12, string.len());

            let expected = "hello world!".as_ptr() as *const i8;
            let value = string.data() as *const i8;
            assert_eq!(0, libc::strncmp(expected, value, string.len()));
        }
    }
}
