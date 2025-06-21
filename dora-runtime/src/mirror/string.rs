use std::ptr;
use std::slice;
use std::str;

use crate::gc::Address;
use crate::mem;
use crate::mirror::{create_handle, Handle, Header, Object, Ref};
use crate::vm::VM;

#[repr(C)]
pub struct Str {
    header: Header,
    length: usize,
    data: u8,
}

impl Str {
    #[allow(dead_code)]
    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn cast(obj: &Object) -> &Str {
        unsafe { &*(obj as *const _ as *const Str) }
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

    pub fn content_utf8(&self) -> &str {
        str::from_utf8(self.content()).expect("invalid encoding")
    }

    /// allocates string from buffer in permanent space
    pub fn from_buffer_in_perm(vm: &VM, buf: &[u8]) -> Ref<Str> {
        let mut handle = str_alloc_perm(vm, buf.len());
        handle.length = buf.len();

        let data = handle.data() as *mut u8;
        unsafe {
            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    /// allocates string from buffer in heap
    pub fn from_buffer(vm: &VM, buf: &[u8]) -> Ref<Str> {
        let mut handle = str_alloc_heap(vm, buf.len());
        handle.length = buf.len();

        let data = handle.data() as *mut u8;
        unsafe {
            // copy buffer content into Str
            ptr::copy_nonoverlapping(buf.as_ptr(), data, buf.len());
        }

        handle
    }

    pub fn from_str(vm: &VM, val: Handle<Str>, offset: usize, len: usize) -> Ref<Str> {
        let total_len = val.len();

        if offset > total_len {
            return Ref::null();
        }

        let len = std::cmp::min(total_len - offset, len);

        let slice = unsafe {
            let data = val.data().offset(offset as isize);
            slice::from_raw_parts(data, len)
        };

        if let Ok(_) = str::from_utf8(slice) {
            let mut handle = str_alloc_heap(vm, len);
            handle.length = len;

            let dest = handle.data() as *mut u8;
            unsafe {
                let src = val.data().offset(offset as isize);

                // copy buffer content into Str
                ptr::copy_nonoverlapping(src, dest, len);
            }

            handle
        } else {
            Ref::null()
        }
    }

    pub fn concat(vm: &VM, lhs: Handle<Str>, rhs: Handle<Str>) -> Handle<Str> {
        let len = lhs.len() + rhs.len();
        let mut handle = create_handle(str_alloc_heap(vm, len));

        handle.length = len;
        unsafe {
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
    pub fn dup(&self, vm: &VM) -> Ref<Str> {
        let len = self.len();
        let mut handle = str_alloc_heap(vm, len);

        handle.length = len;
        unsafe {
            ptr::copy_nonoverlapping(self.data(), handle.data() as *mut u8, len);
        }

        handle
    }
}

fn str_alloc_heap(vm: &VM, len: usize) -> Ref<Str> {
    str_alloc(vm, len, |vm, size| vm.gc.alloc(vm, size), false)
}

fn str_alloc_perm(vm: &VM, len: usize) -> Ref<Str> {
    str_alloc(vm, len, |vm, size| vm.gc.alloc_readonly(vm, size), true)
}

fn str_alloc<F>(vm: &VM, len: usize, alloc: F, is_readonly: bool) -> Ref<Str>
where
    F: FnOnce(&VM, usize) -> Address,
{
    let size = Header::size() as usize      // Object header
                + mem::ptr_width() as usize // length field
                + len; // string content

    let size = mem::align_usize_up(size, mem::ptr_width() as usize);
    let ptr = alloc(vm, size);

    let handle: Ref<Str> = ptr.into();
    let (is_marked, is_remembered) = vm.gc.initial_metadata_value(size, is_readonly);
    handle.header().setup_header_word(
        vm.known.string_shape().address(),
        vm.meta_space_start(),
        is_marked,
        is_remembered,
    );

    handle
}
