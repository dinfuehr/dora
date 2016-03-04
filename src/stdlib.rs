use libc;

use std::ffi::CStr;
use std::io::{self, Write};
use std::os::raw::c_char;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::Str;

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
