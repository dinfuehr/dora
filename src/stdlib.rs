use libc;

use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::os::raw::c_char;
use std::process;
use std::ptr;
use std::slice;
use std::str;
use std::thread;

use ctxt::get_ctxt;
use object::{Handle, Obj, Str};

pub extern "C" fn byte_to_string(val: u8) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
}

pub extern "C" fn char_to_string(val: char) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
}

pub extern "C" fn int_to_string(val: i32) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
}

pub extern "C" fn long_to_string(val: i64) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
}

pub extern "C" fn bool_to_string(val: bool) -> Handle<Str> {
    let val = if val { "true" } else { "false" };
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, val.as_bytes())
}

pub extern "C" fn float_to_string(val: f32) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
}

pub extern "C" fn double_to_string(val: f64) -> Handle<Str> {
    let buffer = val.to_string();
    let ctxt = get_ctxt();

    Str::from_buffer(ctxt, buffer.as_bytes())
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

pub extern "C" fn fatal_error(msg: Handle<Str>) {
    write!(&mut io::stderr(), "fatal error: ").expect("could not print to stderr");

    unsafe {
        let buf = CStr::from_ptr(msg.data() as *const c_char);
        io::stderr().write(buf.to_bytes()).unwrap();
    };

    writeln!(&mut io::stderr(), "").expect("could not print to stderr");
    process::exit(1);
}

pub extern "C" fn abort() {
    writeln!(&mut io::stderr(), "program aborted.").expect("could not print to stderr");
    process::exit(1);
}

pub extern "C" fn exit(status: i32) {
    process::exit(status);
}

pub extern "C" fn println(val: Handle<Str>) {
    print(val);
    println!("");
}

pub extern "C" fn strcmp(lhs: Handle<Str>, rhs: Handle<Str>) -> i32 {
    unsafe {
        libc::strcmp(lhs.data() as *const libc::c_char,
                     rhs.data() as *const libc::c_char)
    }
}

pub extern "C" fn streq(lhs: Handle<Str>, rhs: Handle<Str>) -> bool {
    unsafe {
        libc::strcmp(lhs.data() as *const libc::c_char,
                     rhs.data() as *const libc::c_char) == 0
    }
}

pub extern "C" fn strcat(lhs: Handle<Str>, rhs: Handle<Str>) -> Handle<Str> {
    let ctxt = get_ctxt();

    Str::concat(ctxt, lhs, rhs)
}

pub extern "C" fn gc_alloc(size: usize) -> *mut Obj {
    let ctxt = get_ctxt();
    ctxt.gc.alloc(ctxt, size) as *mut Obj
}

pub extern "C" fn gc_collect() {
    let ctxt = get_ctxt();
    ctxt.gc.collect(ctxt);
}

pub extern "C" fn str_len(s: Handle<Str>) -> i32 {
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

            return Str::from_buffer(ctxt, value.as_bytes());
        }
    }

    panic!("argument does not exist");
}

pub extern "C" fn str_parse_int(val: Handle<Str>) -> i32 {
    let slice = unsafe { slice::from_raw_parts(val.data(), val.len()) };
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}

pub extern "C" fn load_function(name: Handle<Str>) -> usize {
    let name = name.to_cstring();

    unsafe {
        let lib = libc::dlopen(ptr::null(), libc::RTLD_LAZY | libc::RTLD_LOCAL);
        let addr = libc::dlsym(lib, name.as_ptr());

        libc::dlclose(lib);

        addr as usize
    }
}

pub extern "C" fn call0(addr: *const u8) -> usize {
    let fct: extern "C" fn() -> usize = unsafe { mem::transmute(addr) };

    fct()
}

pub extern "C" fn call1(addr: *const u8, arg1: usize) -> usize {
    let fct: extern "C" fn(usize) -> usize = unsafe { mem::transmute(addr) };

    fct(arg1)
}

pub extern "C" fn call2(addr: *const u8, arg1: usize, arg2: usize) -> usize {
    let fct: extern "C" fn(usize, usize) -> usize = unsafe { mem::transmute(addr) };

    fct(arg1, arg2)
}

pub extern "C" fn call3(addr: *const u8, arg1: usize, arg2: usize, arg3: usize) -> usize {
    let fct: extern "C" fn(usize, usize, usize) -> usize = unsafe { mem::transmute(addr) };

    fct(arg1, arg2, arg3)
}

pub extern "C" fn native_malloc(size: usize) -> *const u8 {
    unsafe { libc::malloc(size) as *const u8 }
}

pub extern "C" fn native_free(addr: *const u8) {
    unsafe { libc::free(addr as *mut libc::c_void) }
}

pub extern "C" fn spawn_thread(obj: Handle<Obj>) {
    use baseline;
    use stacktrace::DoraToNativeInfo;

    thread::spawn(move || {
        let ctxt = get_ctxt();

        let main = {
            let cls = obj.header().vtbl().class();
            let name = ctxt.interner.intern("run");
            cls.find_method(ctxt, name, false)
                .expect("run() method not found")
        };

        let fct_ptr = {
            let mut sfi = DoraToNativeInfo::new();
            ctxt.use_sfi(&mut sfi, || baseline::generate(ctxt, main))
        };

        let fct: extern "C" fn(Handle<Obj>) = unsafe { mem::transmute(fct_ptr) };
        fct(obj);
    });
}
