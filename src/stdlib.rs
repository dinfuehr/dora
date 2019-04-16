use libc;

use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::str;
use std::thread;
use std::time::Duration;

use class::TypeParams;
use ctxt::exception_set;
use ctxt::get_vm;
use exception::{alloc_exception, stacktrace_from_last_dtn};
use gc::{Address, GcReason};
use handle::root;
use object::{ByteArray, Obj, Ref, Str};
use os::signal::Trap;
use sym::Sym::SymFct;
use threads::{DoraThread, THREAD};

pub extern "C" fn byte_to_string(val: u8) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn char_to_string(val: char) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn int_to_string(val: i32) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn long_to_string(val: i64) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn float_to_string(val: f32) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn double_to_string(val: f64) -> Ref<Str> {
    let buffer = val.to_string();
    let vm = get_vm();

    Str::from_buffer(vm, buffer.as_bytes())
}

pub extern "C" fn print(val: Ref<Str>) {
    io::stdout().write(val.content()).unwrap();
}

pub extern "C" fn addr(val: Ref<Obj>) -> u64 {
    val.raw() as usize as u64
}

pub extern "C" fn fatal_error(msg: Ref<Str>) {
    write!(&mut io::stderr(), "fatal error: ").expect("could not print to stderr");
    io::stderr().write(msg.content()).unwrap();
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

pub extern "C" fn timestamp() -> u64 {
    use timer;

    timer::timestamp()
}

pub extern "C" fn println(val: Ref<Str>) {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write(val.content()).unwrap();
    handle.write(b"\n").unwrap();
}

pub extern "C" fn sleep(seconds: i32) {
    assert!(seconds >= 0);
    thread::sleep(Duration::from_secs(seconds as u64));
}

pub extern "C" fn throw_native(val: bool) {
    if val {
        let vm = get_vm();
        let obj = alloc_exception(vm, Ref::null());
        let obj = root(obj);

        exception_set(obj.direct().raw() as *const u8);
    }
}

pub extern "C" fn call(fct: Ref<Str>) {
    let fct_name = fct.to_cstring();
    let fct_name = fct_name.to_str().unwrap();

    let vm = get_vm();
    let name = vm.interner.intern(fct_name);

    let sym = vm.sym.lock().get(name);

    match sym {
        Some(SymFct(fct_id)) => {
            {
                let fct = vm.fcts.idx(fct_id);
                let fct = fct.read();

                if !fct.param_types.is_empty() {
                    writeln!(&mut io::stderr(), "fct `{}` takes arguments.", fct_name)
                        .expect("could not print to stderr");
                    process::exit(1);
                }
            }

            vm.run(fct_id);
        }

        _ => {
            writeln!(&mut io::stderr(), "fct `{}` not found.", fct_name)
                .expect("could not print to stderr");
            process::exit(1);
        }
    }
}

pub extern "C" fn strcmp(lhs: Ref<Str>, rhs: Ref<Str>) -> i32 {
    unsafe {
        libc::strcmp(
            lhs.data() as *const libc::c_char,
            rhs.data() as *const libc::c_char,
        )
    }
}

pub extern "C" fn strcat(lhs: Ref<Str>, rhs: Ref<Str>) -> Ref<Str> {
    let vm = get_vm();
    let lhs = root(lhs);
    let rhs = root(rhs);

    Str::concat(vm, lhs, rhs).direct()
}

pub extern "C" fn str_clone(val: Ref<Str>) -> Ref<Str> {
    let vm = get_vm();

    val.dup(vm)
}

pub extern "C" fn str_from_bytes(val: Ref<ByteArray>, offset: usize, len: usize) -> Ref<Str> {
    let vm = get_vm();
    let val: Ref<Str> = val.cast();
    let val = root(val);

    Str::from_str(vm, val, offset, len)
}

pub extern "C" fn gc_verify_refs(obj: Ref<Obj>, value: Ref<Obj>) {
    let vm = get_vm();
    vm.gc.verify_ref(vm, obj.address());
    vm.gc.verify_ref(vm, value.address());
}

pub extern "C" fn gc_alloc(size: usize, array_ref: bool) -> *mut Obj {
    let vm = get_vm();
    vm.gc.alloc(vm, size, array_ref).to_mut_ptr()
}

pub extern "C" fn gc_collect() {
    let vm = get_vm();
    vm.gc.collect(vm, GcReason::ForceCollect);
}

pub extern "C" fn gc_minor_collect() {
    let vm = get_vm();
    vm.gc.minor_collect(vm, GcReason::ForceMinorCollect);
}

pub extern "C" fn str_len(s: Ref<Str>) -> i32 {
    s.len() as i32
}

pub extern "C" fn argc() -> i32 {
    let vm = get_vm();

    if let Some(ref args) = vm.args.arg_argument {
        args.len() as i32
    } else {
        0
    }
}

pub extern "C" fn argv(ind: i32) -> Ref<Str> {
    let vm = get_vm();

    if let Some(ref args) = vm.args.arg_argument {
        if ind >= 0 && ind < args.len() as i32 {
            let value = &args[ind as usize];

            return Str::from_buffer(vm, value.as_bytes());
        }
    }

    panic!("argument does not exist");
}

pub extern "C" fn str_parse_int(val: Ref<Str>) -> i32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}

pub extern "C" fn str_parse_long(val: Ref<Str>) -> i64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().unwrap_or(0)
}

pub extern "C" fn load_function(name: Ref<Str>) -> usize {
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

pub extern "C" fn trap(trap_id: u32) {
    let vm = get_vm();
    let trap = Trap::from(trap_id).expect("invalid trap id!");

    let msg = match trap {
        Trap::DIV0 => "division by 0",
        Trap::ASSERT => "assert failed",
        Trap::INDEX_OUT_OF_BOUNDS => "array index out of bounds",
        Trap::NIL => "nil check failed",
        Trap::CAST => "cast failed",
        Trap::THROW => "uncaught exception",
        Trap::UNEXPECTED => "unexpected exception",
        Trap::OOM => "out of memory",
    };

    println!("{}", msg);
    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump(vm);
    unsafe {
        libc::_exit(100 + trap_id as i32);
    }
}

pub extern "C" fn spawn_thread(obj: Ref<Obj>) {
    use baseline;
    use exception::DoraToNativeInfo;

    let vm = get_vm();
    let thread = DoraThread::new();

    vm.threads.attach_thread(thread.clone());

    thread::spawn(move || {
        THREAD.with(|tld| {
            *tld.borrow_mut() = thread;
        });

        let main = {
            let cls_id = obj.header().vtbl().class().cls_id;
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            let name = vm.interner.intern("run");
            cls.find_method(vm, name, false)
                .expect("run() method not found")
        };

        let tld = THREAD.with(|thread| {
            let thread = thread.borrow();
            let ptr = &thread.tld;

            Address::from_ptr(ptr as *const _)
        });

        let fct_ptr = {
            let mut dtn = DoraToNativeInfo::new();
            let type_params = TypeParams::empty();

            THREAD.with(|thread| {
                thread.borrow().use_dtn(&mut dtn, || {
                    baseline::generate(vm, main, &type_params, &type_params)
                })
            })
        };

        // execute the thread object's run-method
        let dora_entry_thunk = vm.dora_entry_thunk();
        let fct: extern "C" fn(Address, Address, Ref<Obj>) =
            unsafe { mem::transmute(dora_entry_thunk) };
        fct(tld, fct_ptr, obj);

        // remove thread from list of all threads
        vm.threads.detach_current_thread();
    });
}
