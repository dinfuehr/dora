use libc;

use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::os::raw::c_char;
use std::slice;
use std::str;
use ctxt::get_ctxt;
use mem::ptr::Ptr;
use object::{Handle, IntArray, Obj, Str};

pub extern "C" fn assert(val: bool) {
    if !val {
        unsafe {
            println!("assert failed");
            libc::_exit(101);
        }
    }
}

pub extern "C" fn throw_exception(obj: Handle<Obj>) {
    unsafe { libc::_exit(104); }
}

pub extern "C" fn int_to_string(val: i32) -> Handle<Str> {
    let buffer = val.to_string();
    Str::from(buffer.as_bytes())
}

pub extern "C" fn bool_to_string(val: bool) -> Handle<Str> {
    let val = if val {
        "true"
    } else {
        "false"
    };

    Str::from(val.as_bytes())
}

pub extern "C" fn bool_to_int(val: bool) -> i32 {
    if val { 1 } else { 0 }
}

pub extern "C" fn print(val: Handle<Str>) {
    unsafe {
        let buf = CStr::from_ptr(val.data() as *const c_char);
        io::stdout().write(buf.to_bytes()).unwrap();
    };
}

pub extern "C" fn println(val: Handle<Str>) {
    print(val);
    println!("");
}

pub extern "C" fn strcmp(lhs: Handle<Str>, rhs: Handle<Str>) -> i32 {
    unsafe {
        libc::strcmp(lhs.data() as *const i8, rhs.data() as *const i8)
    }
}

pub extern "C" fn strcat(lhs: Handle<Str>, rhs: Handle<Str>) -> Handle<Str> {
    Str::concat(lhs, rhs)
}

pub extern "C" fn gc_alloc(size: usize) -> Ptr {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();

    gc.alloc(size)
}

pub extern "C" fn gc_collect() {
    let ctxt = get_ctxt();
    let mut gc = ctxt.gc.lock().unwrap();

    gc.collect();
}

pub extern "C" fn ctor_int_array_empty() -> Handle<IntArray> {
    IntArray::alloc_with_elem(0, 0)
}

pub extern "C" fn ctor_int_array_elem(len: i32, value: i32) -> Handle<IntArray> {
    IntArray::alloc_with_elem(len as usize, value)
}

pub extern "C" fn int_array_len(array: Handle<IntArray>) -> i32 {
    array.len() as i32
}

pub extern "C" fn str_array_len(s: Handle<Str>) -> i32 {
    s.len() as i32
}

pub extern "C" fn argc() -> i32 {
    let ctxt = get_ctxt();

    if let Some(ref args) = ctxt.args.arg_argument {
        args.len() as i32
    } else {
        0
    }
}

pub extern "C" fn argv(ind: i32) -> Handle<Str> {
    let ctxt = get_ctxt();

    if let Some(ref args) = ctxt.args.arg_argument {
        if ind >= 0 && ind < args.len() as i32 {
            let value = &args[ind as usize];

            return Str::from(value.as_bytes());
        }
    }

    panic!("argument does not exist");
}

pub extern "C" fn parse(val: Handle<Str>) -> i32 {
    let slice = unsafe { slice::from_raw_parts(val.data(), val.len()) };
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}
