use libc;

use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::os::raw::c_char;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::{IntArray, Str};

pub extern "C" fn assert(val: bool) {
    if !val {
        unsafe {
            println!("assert failed");
            libc::_exit(101);
        }
    }
}

pub extern "C" fn to_string(val: i32) -> Str {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();
    let buffer = val.to_string();

    Str::from_buffer(&mut gc, buffer.as_bytes())

}

pub extern "C" fn print(val: Str) {
    unsafe {
        let buf = CStr::from_ptr(val.data() as *const c_char);
        io::stdout().write(buf.to_bytes()).unwrap();
    };
}

pub extern "C" fn println(val: Str) {
    print(val);
    println!("");
}

pub extern "C" fn strcmp(lhs: Str, rhs: Str) -> i32 {
    unsafe {
        libc::strcmp(lhs.data() as *const i8, rhs.data() as *const i8)
    }
}

pub extern "C" fn strcat(lhs: Str, rhs: Str) -> Str {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();

    Str::concat(&mut gc, lhs, rhs)
}

pub extern "C" fn gc_alloc(size: usize) -> Ptr {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();

    gc.alloc(size)
}

pub extern "C" fn int_array_empty(ptr: Ptr) -> Ptr {
    let data = IntArray::empty();

    unsafe { *(ptr.raw() as *mut IntArray) = data; }

    ptr
}

pub extern "C" fn int_array_elem(ptr: Ptr, len: i32, value: i32) -> Ptr {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();

    let data = IntArray::with_element(&mut gc, len as usize, value as isize);

    unsafe { *(ptr.raw() as *mut IntArray) = data; }

    ptr
}

pub extern "C" fn int_array_len(ptr: *const IntArray) -> i32 {
    let array = unsafe { &*ptr };

    array.len() as i32
}

pub extern "C" fn int_array_get(ptr: *const IntArray, ind: i32) -> i32 {
    let array = unsafe { &*ptr };

    array.get(ind)
}
