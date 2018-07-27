use std::ffi::{CStr};

use libc;

pub mod fct;


type EmitResult<T> = Result<T, ()>;

fn fail<T>() -> EmitResult<T> {
    Err(())
}

fn ok<T>(value: T) -> EmitResult<T> {
    Ok(value)
}

fn noname() -> *const i8 {
    b"\0".as_ptr() as *const _
}

extern "C" fn resolver(name: *const i8, _: *mut libc::c_void) -> u64 {
    let name = unsafe { CStr::from_ptr(name) };
    panic!("resolver unimplemented: {:?}", name);
}
