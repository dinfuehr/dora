use std;
use std::ptr;

use class::Class;
use gc::Gc;
use mem;
use mem::ptr::Ptr;

pub struct Header {
    class: *mut Class<'static>,
}

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
        let string = Str::new(gc, buf.len());

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

    pub fn new(gc: &mut Gc, len: usize) -> Str {
        let size = mem::ptr_width() as usize + len + 1;

        Str { ptr: gc.alloc(size) }
    }

    pub fn concat(gc: &mut Gc, lhs: Str, rhs: Str) -> Str {
        let len = lhs.len() + rhs.len();
        let string = Str::new(gc, len);

        unsafe {
            string.set_len(len);

            ptr::copy_nonoverlapping(lhs.data(), string.data(), lhs.len());
            ptr::copy_nonoverlapping(rhs.data(),
                string.data().offset(lhs.len() as isize), rhs.len());

            *(string.data().offset(len as isize)) = 0;
        }

        string
    }
}

pub struct IntArray {
    ptr: *mut i32,
    length: usize,
}

impl IntArray {
    pub fn empty() -> IntArray {
        IntArray {
            ptr: ptr::null_mut(),
            length: 0,
        }
    }

    pub fn with_element(gc: &mut Gc, length: usize, elem: isize) -> IntArray {
        if length < 0 {
            panic!("length needs to be greater or equal to 0.");
        }

        let ptr = if length > 0 {
            gc.alloc(std::mem::size_of::<i32>() * length).raw() as *mut i32
        } else {
            ptr::null_mut()
        };

        for i in 0..length {
            unsafe {
                *ptr.offset(i as isize) = elem as i32;
            }
        }

        IntArray {
            ptr: ptr,
            length: length,
        }
    }

    pub fn size() -> usize {
        std::mem::size_of::<IntArray>()
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn get(&self, ind: i32) -> i32 {
        if ind < 0 || ind as usize >= self.length {
            panic!("index out of bounds");
        }

        unsafe {
            *self.ptr.offset(ind as isize)
        }
    }

    pub fn set(&self, ind: i32, value: i32) {
        if ind < 0 || ind as usize >= self.length {
            panic!("index out of bounds");
        }

        unsafe {
            *self.ptr.offset(ind as isize) = value;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use gc::Gc;
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
