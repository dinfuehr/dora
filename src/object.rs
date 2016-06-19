use libc;

use std;
use std::ops::{Deref, DerefMut};
use std::ptr;

use class::Class;
use ctxt::get_ctxt;
use mem;
use mem::ptr::Ptr;
use ty::BuiltinType;

pub struct Header {
    // ptr to class
    class: *const Class<'static>,

    // additional information>
    // bit 0 - marked flag
    info: usize,

    // next allocated object
    next: *mut Obj,
}

impl Header {
    pub fn size() -> i32 {
        std::mem::size_of::<Header>() as i32
    }

    pub fn classptr(&self) -> usize {
        self.class as usize
    }

    pub fn class(&self) -> &Class<'static> {
        unsafe { &(*self.class) }
    }

    pub fn set_mark(&mut self, value: bool) {
        self.info = if value {
            self.info | 1
        } else {
            self.info & (!1)
        };
    }

    pub fn marked(&self) -> bool {
        if (self.info & 1) != 0 {
            true
        } else {
            false
        }
    }

    pub fn set_succ(&mut self, ptr: *mut Obj) {
        self.next = ptr;
    }

    pub fn succ(&self) -> *mut Obj {
        self.next
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

    pub fn size(&self) -> usize {
        let size = self.header().class().size;
        if size > 0 { return size as usize; }

        let ty = self.header().class().ty;

        match ty {
            BuiltinType::Str => {
                let handle: Handle<Str> = Handle {
                    ptr: self as *const Obj as *const Str
                };
                handle.size()
            }

            BuiltinType::IntArray => {
                let handle: Handle<IntArray> = Handle {
                    ptr: self as *const Obj as *const IntArray
                };
                handle.size()
            }

            _ => panic!("size unknown")
        }
    }
}

pub struct Handle<T> {
    ptr: *const T
}

impl<T> Handle<T> {
    pub fn raw(&self) -> *const T {
        self.ptr
    }
}

// known limitation of #[derive(Copy, Clone)]
// traits need to be implemented manually
impl<T> Copy for Handle<T> {}
impl<T> Clone for Handle<T> {
    fn clone(&self) -> Handle<T> {
        *self
    }
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

    pub fn size(&self) -> usize {
        Header::size() as usize         // Object header
            + mem::ptr_width() as usize // length field
            + self.len() + 1   // string content
    }

    pub fn alloc(len: usize) -> Handle<Str> {
        let size = Header::size() as usize     // Object header
                   + mem::ptr_width() as usize // length field
                   + len + 1;                  // string content

        let ctxt = get_ctxt();
        let ptr = ctxt.gc.lock().unwrap().alloc(size) as usize;

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
    header: Header,
    length: usize,
    data: u8
}

impl IntArray {
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn data(&self) -> *const i32 {
        &self.data as *const u8 as *const i32
    }

    pub fn data_mut(&mut self) -> *mut i32 {
        &self.data as *const u8 as *mut i32
    }

    pub fn size(&self) -> usize {
        Header::size() as usize         // Object header
            + mem::ptr_width() as usize // length field
            + self.len() * std::mem::size_of::<i32>() // array content
    }

    pub fn alloc_empty() -> Handle<IntArray> {
        IntArray::alloc_with_elem(0, 0)
    }

    pub fn alloc_with_elem(len: usize, elem: i32) -> Handle<IntArray> {
        let size = Header::size() as usize        // Object header
                   + mem::ptr_width() as usize    // length field
                   + len * std::mem::size_of::<i32>(); // array content

        let ctxt = get_ctxt();
        let ptr = ctxt.gc.lock().unwrap().alloc(size) as usize;

        let cls = ctxt.primitive_classes.int_array_classptr;
        let mut handle : Handle<IntArray> = ptr.into();
        handle.header_mut().class = cls as *const Class;
        handle.length = len;

        for i in 0..handle.len() {
            unsafe { *handle.data_mut().offset(i as isize) = elem; }
        }

        handle
    }

    pub fn offset_of_length() -> i32 {
        Header::size()
    }

    pub fn offset_of_data() -> i32 {
        Header::size() + mem::ptr_width()
    }
}
