use std;
use std::ffi::CString;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::ptr;
use std::slice;
use std::str;

use class::{ClassDefId, ClassSize};
use ctxt::SemContext;
use gc::Address;
use gc::root::IndirectObj;
use handle::Rooted;
use mem;
use vtable::VTable;

#[repr(C)]
pub struct Header {
    // ptr to class
    vtable: *mut VTable,
}

impl Header {
    pub fn size() -> i32 {
        std::mem::size_of::<Header>() as i32
    }

    pub fn vtblptr(&self) -> *mut VTable {
        self.vtable
    }

    pub fn vtbl(&self) -> &mut VTable {
        unsafe { &mut *self.vtable }
    }

    pub fn forward_to(&mut self, address: Address) {
        self.vtable = (address.to_usize() | 1) as *mut VTable;
    }

    pub fn forwarded(&self) -> Option<Address> {
        let addr = self.vtable as usize;

        if (addr & 1) == 1 {
            Some((addr & !1).into())
        } else {
            None
        }
    }
}

// is used to reference any object
#[repr(C)]
pub struct Obj {
    header: Header,
    data: u8,
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

    pub fn is_array_ref(&self) -> bool {
        let cls = self.header().vtbl().class();

        match cls.size {
            ClassSize::ObjArray => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        let cls = self.header().vtbl().class();

        match cls.size {
            ClassSize::Fixed(size) => size as usize,

            ClassSize::ObjArray => determine_array_size(self, mem::ptr_width()),

            ClassSize::Array(element_size) => determine_array_size(self, element_size),

            ClassSize::Str => {
                let handle: Handle<Str> = Handle { ptr: self as *const Obj as *const Str };
                mem::align_usize(handle.size(), mem::ptr_width() as usize)
            }
        }
    }

    pub fn visit_reference_fields<F>(&mut self, mut f: F)
    where
        F: FnMut(IndirectObj),
    {
        let classptr = self.header().vtbl().classptr;
        let cls = unsafe { &*classptr };

        if let ClassSize::ObjArray = cls.size {
            let array = unsafe { &*(self as *const _ as *const StrArray) };

            // walk through all objects in array
            let mut ptr = array.data() as *mut *mut Obj;
            let last = unsafe { ptr.offset(array.len() as isize) };

            while ptr < last {
                f((ptr as usize).into());

                unsafe { ptr = ptr.offset(1) }
            }

            return;
        }

        for &offset in &cls.ref_fields {
            let obj = (self as *mut Obj as usize) + offset as usize;
            f(obj.into());
        }
    }

    pub fn copy_to(&self, dest: Address, size: usize) {
        unsafe {
            ptr::copy_nonoverlapping(
                self as *const Obj as *const u8,
                dest.to_mut_ptr::<u8>(),
                size,
            );
        }
    }
}

fn determine_array_size(obj: &Obj, element_size: i32) -> usize {
    let handle: Handle<ByteArray> = Handle { ptr: obj as *const Obj as *const ByteArray };

    let value = Header::size() as usize + mem::ptr_width() as usize +
        element_size as usize * handle.len() as usize;

    mem::align_usize(value, mem::ptr_width() as usize)
}

#[repr(C)]
pub struct Handle<T> {
    ptr: *const T,
}

unsafe impl<T> Send for Handle<T> {}

impl<T> Handle<T> {
    pub fn null() -> Handle<T> {
        Handle { ptr: ptr::null() }
    }

    pub fn cast<R>(&self) -> Handle<R> {
        Handle { ptr: self.ptr as *const R }
    }

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
        Handle { ptr: self as *const T }
    }
}

#[repr(C)]
pub struct Testing {
    header: Header,
    failed: bool,
}

impl Testing {
    pub fn has_failed(&self) -> bool {
        self.failed
    }
}

#[repr(C)]
pub struct Str {
    header: Header,
    length: usize,
    data: u8,
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

    pub fn content(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.data(), self.len()) }
    }

    pub fn to_cstring(&self) -> CString {
        let view = self.content();

        CString::new(view).unwrap()
    }

    pub fn size(&self) -> usize {
        Header::size() as usize         // Object header
            + mem::ptr_width() as usize // length field
            + self.len() // string content
    }

    /// allocates string from buffer in permanent space
    pub fn from_buffer_in_perm(ctxt: &SemContext, buf: &[u8]) -> Handle<Str> {
        let mut handle = str_alloc_perm(ctxt, buf.len());
        handle.length = buf.len();

        unsafe {
            let data = handle.data() as *mut u8;

            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    /// allocates string from buffer in heap
    pub fn from_buffer(ctxt: &SemContext, buf: &[u8]) -> Handle<Str> {
        let mut handle = str_alloc_heap(ctxt, buf.len());
        handle.length = buf.len();

        unsafe {
            let data = handle.data() as *mut u8;

            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    pub fn from_str(ctxt: &SemContext, val: Rooted<Str>, offset: usize, len: usize) -> Handle<Str> {
        let total_len = val.len();

        if offset > total_len {
            return Handle::null();
        }

        let len = std::cmp::min(total_len - offset, len);

        let slice = unsafe {
            let data = val.data().offset(offset as isize);
            slice::from_raw_parts(data, len)
        };

        if let Ok(_) = str::from_utf8(slice) {
            let mut handle = str_alloc_heap(ctxt, len);
            handle.length = len;

            unsafe {
                let dest = handle.data() as *mut u8;
                let src = val.data().offset(offset as isize);

                // copy buffer content into Str
                ptr::copy_nonoverlapping(src, dest, len);
            }

            handle
        } else {
            Handle::null()
        }
    }

    pub fn concat(ctxt: &SemContext, lhs: Rooted<Str>, rhs: Rooted<Str>) -> Rooted<Str> {
        let len = lhs.len() + rhs.len();
        let mut handle = ctxt.handles.root(str_alloc_heap(ctxt, len));

        unsafe {
            handle.length = len;

            ptr::copy_nonoverlapping(lhs.data(), handle.data() as *mut u8, lhs.len());
            ptr::copy_nonoverlapping(
                rhs.data(),
                handle.data().offset(lhs.len() as isize) as *mut u8,
                rhs.len(),
            );
        }

        handle
    }

    // duplicate string into a new object
    pub fn dup(&self, ctxt: &SemContext) -> Handle<Str> {
        let len = self.len();
        let mut handle = str_alloc_heap(ctxt, len);

        unsafe {
            handle.length = len;

            ptr::copy_nonoverlapping(self.data(), handle.data() as *mut u8, len);
        }

        handle
    }
}

fn str_alloc_heap(ctxt: &SemContext, len: usize) -> Handle<Str> {
    str_alloc(ctxt, len, |ctxt, size| {
        ctxt.gc.alloc(ctxt, size, false) as *const u8
    })
}

fn str_alloc_perm(ctxt: &SemContext, len: usize) -> Handle<Str> {
    str_alloc(ctxt, len, |ctxt, size| ctxt.gc.alloc_perm(size))
}

fn str_alloc<F>(ctxt: &SemContext, len: usize, alloc: F) -> Handle<Str>
where
    F: FnOnce(&SemContext, usize) -> *const u8,
{
    let size = Header::size() as usize      // Object header
                + mem::ptr_width() as usize // length field
                + len; // string content

    let size = mem::align_usize(size, mem::ptr_width() as usize);
    let ptr = alloc(ctxt, size) as usize;

    let clsid = ctxt.vips.str(ctxt);
    let cls = ctxt.class_defs[clsid].borrow();
    let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();
    let mut handle: Handle<Str> = ptr.into();
    handle.header_mut().vtable = vtable as *mut VTable;

    handle
}

#[repr(C)]
pub struct Array<T: Copy> {
    header: Header,
    length: usize,
    data: u8,
    phantom: PhantomData<T>,
}

impl<T> Array<T>
where
    T: Copy,
{
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn data(&self) -> *const T {
        &self.data as *const u8 as *const T
    }

    pub fn data_mut(&mut self) -> *mut T {
        &self.data as *const u8 as *mut T
    }

    pub fn get_at(&self, idx: usize) -> T {
        unsafe { *self.data().offset(idx as isize) }
    }

    pub fn set_at(&mut self, idx: usize, val: T) {
        unsafe {
            *self.data_mut().offset(idx as isize) = val;
        }
    }

    pub fn size(&self) -> usize {
        Header::size() as usize         // Object header
            + mem::ptr_width() as usize // length field
            + self.len() * std::mem::size_of::<T>() // array content
    }

    pub fn alloc(ctxt: &SemContext, len: usize, elem: T, clsid: ClassDefId) -> Handle<Array<T>> {
        let size = Header::size() as usize        // Object header
                   + mem::ptr_width() as usize    // length field
                   + len * std::mem::size_of::<T>(); // array content

        let ptr = ctxt.gc.alloc(ctxt, size, false) as usize;
        let cls = ctxt.class_defs[clsid].borrow();
        let vtable: *const VTable = &**cls.vtable.as_ref().unwrap();
        let mut handle: Handle<Array<T>> = ptr.into();
        handle.header_mut().vtable = vtable as *mut VTable;
        handle.length = len;

        for i in 0..handle.len() {
            unsafe {
                *handle.data_mut().offset(i as isize) = elem;
            }
        }

        handle
    }
}

pub fn offset_of_array_length() -> i32 {
    offset_of!(Array<i32>, length) as i32
}

pub fn offset_of_array_data() -> i32 {
    offset_of!(Array<i32>, data) as i32
}

pub type BoolArray = Array<bool>;
pub type ByteArray = Array<u8>;
pub type CharArray = Array<char>;
pub type IntArray = Array<i32>;
pub type LongArray = Array<i64>;
pub type FloatArray = Array<f32>;
pub type DoubleArray = Array<f64>;
pub type StrArray = Array<Handle<Str>>;

pub fn alloc(ctxt: &SemContext, clsid: ClassDefId) -> Handle<Obj> {
    let cls_def = ctxt.class_defs[clsid].borrow();

    let size = match cls_def.size {
        ClassSize::Fixed(size) => size as usize,
        _ => panic!("alloc only supports fix-sized types"),
    };

    let size = mem::align_usize(size, mem::ptr_width() as usize);

    let ptr = ctxt.gc.alloc(ctxt, size, false) as usize;
    let vtable: *const VTable = &**cls_def.vtable.as_ref().unwrap();
    let mut handle: Handle<Obj> = ptr.into();
    handle.header_mut().vtable = vtable as *mut VTable;

    handle
}

pub struct Exception {
    pub header: Header,
    pub msg: Handle<Str>,
    pub backtrace: Handle<IntArray>,
    pub elements: Handle<Obj>,
}

pub struct StackTraceElement {
    pub header: Header,
    pub name: Handle<Str>,
    pub line: i32,
}
