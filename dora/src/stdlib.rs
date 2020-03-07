use libc;

use std::io::{self, Write};
use std::mem;
use std::process;
use std::str;
use std::thread;
use std::time::Duration;

use crate::gc::{Address, GcReason};
use crate::handle::{scope as handle_scope, Handle};
use crate::object::{ByteArray, Obj, Ref, Str};
use crate::stack::stacktrace_from_last_dtn;
use crate::sym::Sym::SymFct;
use crate::threads::{DoraThread, STACK_SIZE, THREAD};
use crate::ty::TypeList;
use crate::vm::{get_vm, stack_pointer, Trap};

pub extern "C" fn byte_to_string(val: u8) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn char_to_string(val: char) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn int_to_string(val: i32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn long_to_string(val: i64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn float_to_string(val: f32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn double_to_string(val: f64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn print(val: Handle<Str>) {
    io::stdout().write(val.content()).unwrap();
}

pub extern "C" fn addr(val: Handle<Obj>) -> u64 {
    val.raw() as usize as u64
}

pub extern "C" fn fatal_error(msg: Handle<Str>) {
    eprint!("fatal error: ");
    io::stderr().write(msg.content()).unwrap();
    eprintln!("");

    let vm = get_vm();
    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump_err(vm);

    process::exit(1);
}

pub extern "C" fn abort() {
    eprintln!("program aborted.");
    process::exit(1);
}

pub extern "C" fn exit(status: i32) {
    process::exit(status);
}

pub extern "C" fn timestamp() -> u64 {
    use crate::timer;

    timer::timestamp()
}

pub extern "C" fn println(val: Handle<Str>) {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write(val.content()).unwrap();
    handle.write(b"\n").unwrap();
}

pub extern "C" fn sleep(seconds: i32) {
    assert!(seconds >= 0);
    thread::sleep(Duration::from_secs(seconds as u64));
}

pub extern "C" fn call(fct: Handle<Str>) {
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

pub extern "C" fn strcmp(lhs: Handle<Str>, rhs: Handle<Str>) -> i32 {
    unsafe {
        libc::strcmp(
            lhs.data() as *const libc::c_char,
            rhs.data() as *const libc::c_char,
        )
    }
}

pub extern "C" fn strcat(lhs: Handle<Str>, rhs: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();
        Str::concat(vm, lhs, rhs).direct()
    })
}

pub extern "C" fn str_clone(val: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();

        val.dup(vm)
    })
}

pub extern "C" fn str_from_bytes(val: Handle<ByteArray>, offset: usize, len: usize) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();
        let val: Handle<Str> = val.cast();

        Str::from_str(vm, val, offset, len)
    })
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

pub extern "C" fn str_to_int_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().is_ok()
}

pub extern "C" fn str_to_int(val: Handle<Str>) -> i32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}

pub extern "C" fn str_to_long_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().is_ok()
}

pub extern "C" fn str_to_long(val: Handle<Str>) -> i64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().unwrap_or(0)
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
        Trap::UNEXPECTED => "unexpected exception",
        Trap::OOM => "out of memory",
        Trap::STACK_OVERFLOW => "stack overflow",
    };

    eprintln!("{}", msg);
    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump_err(vm);
    unsafe {
        libc::_exit(100 + trap_id as i32);
    }
}

pub extern "C" fn spawn_thread(obj: Handle<Obj>) {
    use crate::compiler;
    use crate::stack::DoraToNativeInfo;

    let vm = get_vm();
    let thread = DoraThread::new(vm);

    vm.threads.attach_thread(thread.clone());
    let obj = obj.direct();

    thread::spawn(move || {
        THREAD.with(|tld_thread| {
            *tld_thread.borrow_mut() = thread;
        });

        let stack_top = stack_pointer();
        let stack_limit = stack_top.sub(STACK_SIZE);

        THREAD.with(|thread| {
            thread.borrow().tld.set_stack_limit(stack_limit);
        });

        let main = {
            let cls_id = obj.header().vtbl().class().cls_id;
            let cls_id = cls_id.expect("no corresponding class");
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
            let type_params = TypeList::empty();

            THREAD.with(|thread| {
                thread.borrow().use_dtn(&mut dtn, || {
                    compiler::generate(vm, main, &type_params, &type_params)
                })
            })
        };

        // execute the thread object's run-method
        let dora_stub_address = vm.dora_stub();
        let fct: extern "C" fn(Address, Address, Ref<Obj>) =
            unsafe { mem::transmute(dora_stub_address) };
        fct(tld, fct_ptr, obj);

        // remove thread from list of all threads
        vm.threads.detach_current_thread();
    });
}
