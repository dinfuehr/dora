use libc;

use std;
use std::ops::{Deref, DerefMut};
use std::ops::Index;
use std::ptr;

use class::Class;
use ctxt::get_ctxt;
use gc::Gc;
use mem;
use mem::ptr::Ptr;

pub struct Header {
    // ptr to class
    class: *const Class<'static>,

    // additional information>
    // bit 0 - marked flag
    info: usize,
}

impl Header {
    pub fn size() -> i32 {
        std::mem::size_of::<Header>() as i32
    }

    pub fn classptr(&self) -> usize {
        self.class as usize
    }

    pub fn class(&self) -> &Class {
        unsafe { &(*self.class) }
    }

    pub fn unmark(&mut self) {
        self.info = self.info & (!1);
    }

    pub fn mark(&mut self) {
        self.info = self.info | 1;
    }

    pub fn is_marked(&self) -> bool {
        if (self.info & 1) != 0 {
            true
        } else {
            false
        }
    }
}

// is used to reference any object
pub struct Obj {
    header: Header,
    data: u8
}

impl Obj {
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn data(&self) -> *const u8 {
        &self.data as *const u8
    }
}

pub struct Handle<T> {
    ptr: *const T
}

impl<T> Deref for Handle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<T> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *(self.ptr as *mut T) }
    }
}

impl<T> Into<Handle<T>> for usize {
    fn into(self) -> Handle<T> {
        Handle {
            ptr: self as *const T
        }
    }
}

impl<T> Into<Ptr> for Handle<T> {
    fn into(self) -> Ptr {
        Ptr::new(self.ptr as *mut libc::c_void)
    }
}

pub struct Str {
    header: Header,
    length: usize,
    data: u8
}

impl Str {
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn data(&self) -> *const u8 {
        &self.data as *const u8
    }

    pub fn alloc(len: usize) -> Handle<Str> {
        let size = Header::size() as usize     // Object header
                   + mem::ptr_width() as usize // length field
                   + len + 1;                  // string content

        let ctxt = get_ctxt();
        let ptr = ctxt.gc.lock().unwrap().alloc(size).raw() as usize;

        let cls = ctxt.primitive_classes.str_classptr;
        let mut handle : Handle<Str> = ptr.into();
        handle.header_mut().class = cls as *const Class;

        handle
    }

    pub fn from(buf: &[u8]) -> Handle<Str> {
        let mut handle = Str::alloc(buf.len());
        handle.length = buf.len();

        unsafe {
            let data = handle.data() as *mut u8;

            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());

            // string should end with 0 for C compatibility
            *(data.offset(buf.len() as isize)) = 0;
        }

        handle
    }

    pub fn concat(lhs: Handle<Str>, rhs: Handle<Str>) -> Handle<Str> {
        let len = lhs.len() + rhs.len();
        let mut handle = Str::alloc(len);

        unsafe {
            handle.length = len;

            ptr::copy_nonoverlapping(lhs.data(), handle.data() as *mut u8, lhs.len());
            ptr::copy_nonoverlapping(rhs.data(),
                handle.data().offset(lhs.len() as isize) as *mut u8, rhs.len());

            *(handle.data().offset(len as isize) as *mut u8) = 0;
        }

        handle
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
