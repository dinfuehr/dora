use libc;

use std::char;
use std::io::Write;
use std::mem;
use std::str;
use std::thread;
use std::time::Duration;

use crate::gc::{Address, GcReason};
use crate::handle::{handle, handle_scope, Handle};
use crate::object::{Obj, Ref, Str, UInt8Array};
use crate::stack::stacktrace_from_last_dtn;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ManagedThread,
    ThreadState, STACK_SIZE,
};
use crate::vm::{get_vm, stack_pointer, ManagedCondition, ManagedMutex, ShapeKind, Trap};

pub mod io;

pub extern "C" fn uint8_to_string(val: u8) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn char_to_string(val: u32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = unsafe { char::from_u32_unchecked(val) }.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn int32_to_string(val: i32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn int64_to_string(val: i64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn float32_to_string(val: f32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn float64_to_string(val: f64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

pub extern "C" fn print(val: Handle<Str>) {
    std::io::stdout().write(val.content()).unwrap();
}

pub extern "C" fn fatal_error(msg: Handle<Str>) {
    eprint!("fatal error: ");
    std::io::stderr().write(msg.content()).unwrap();
    eprintln!("");

    let vm = get_vm();
    let stacktrace = stacktrace_from_last_dtn(vm);
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    stacktrace.dump(vm, &mut stderr).expect("output broken");

    std::process::exit(1);
}

pub extern "C" fn abort() {
    eprintln!("program aborted.");
    std::process::exit(1);
}

pub extern "C" fn exit(status: i32) {
    std::process::exit(status);
}

pub extern "C" fn unreachable() {
    let vm = get_vm();

    eprintln!("unreachable code executed.");

    let stacktrace = stacktrace_from_last_dtn(vm);
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    stacktrace.dump(vm, &mut stderr).expect("output broken");

    std::process::exit(1);
}

pub extern "C" fn timestamp() -> u64 {
    use crate::timer;

    timer::timestamp()
}

pub extern "C" fn println(val: Handle<Str>) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    handle.write(val.content()).unwrap();
    handle.write(b"\n").unwrap();
}

pub extern "C" fn sleep(seconds: i32) {
    assert!(seconds >= 0);
    thread::sleep(Duration::from_secs(seconds as u64));
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

pub extern "C" fn str_from_bytes(val: Handle<UInt8Array>, offset: usize, len: usize) -> Ref<Str> {
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

pub extern "C" fn str_to_int32_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().is_ok()
}

pub extern "C" fn str_to_int32(val: Handle<Str>) -> i32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}

pub extern "C" fn str_to_int64_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().is_ok()
}

pub extern "C" fn str_to_int64(val: Handle<Str>) -> i64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().unwrap_or(0)
}

pub extern "C" fn str_to_float32_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f32>().is_ok()
}

pub extern "C" fn str_to_float32(val: Handle<Str>) -> f32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f32>().unwrap_or(0.0f32)
}

pub extern "C" fn str_to_float64_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f64>().is_ok()
}

pub extern "C" fn str_to_float64(val: Handle<Str>) -> f64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f64>().unwrap_or(0.0)
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
        Trap::OOM => "out of memory",
        Trap::STACK_OVERFLOW => "stack overflow",
        Trap::ILLEGAL => "illegal state",
        Trap::OVERFLOW => "overflow",
    };

    eprintln!("{}", msg);
    let stacktrace = stacktrace_from_last_dtn(vm);
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    stacktrace.dump(vm, &mut stderr).expect("output broken");
    unsafe {
        libc::_exit(100 + trap_id as i32);
    }
}

pub extern "C" fn spawn_thread(runner: Handle<Obj>) -> Address {
    let vm = get_vm();

    handle_scope(|| {
        let managed_thread = ManagedThread::alloc(vm);
        let mut managed_thread: Handle<ManagedThread> = handle(managed_thread);

        // Create new thread in Parked state.
        let thread = DoraThread::new(vm, ThreadState::Parked);

        managed_thread.install_native_thread(&thread);

        vm.gc
            .add_finalizer(managed_thread.direct_ptr(), thread.clone());

        // Add thread to our list of all threads first. This method parks
        // and unparks the current thread, this means the handle needs to be created
        // afterwards.
        vm.threads.add_thread(thread.clone());

        // Now we can create a handle in that newly created thread. Since the thread
        // is now registered, the handle is updated as well by the GC.
        // We create the handle in the new Parked thread, normally this would be unsafe.
        // Here it should be safe though, because the current thread is still Running
        // and therefore the GC can't run at this point.
        debug_assert!(current_thread().is_running());
        let thread_location = thread.handles.handle(managed_thread.direct()).location();
        let runner_location = thread.handles.handle(runner.direct()).location();

        thread::spawn(move || {
            // Initialize thread-local variable with thread
            let thread = init_current_thread(thread);
            thread_main(thread, thread_location, runner_location);
            deinit_current_thread();
        });

        managed_thread.direct_ptr()
    })
}

fn thread_main(thread: &DoraThread, thread_location: Address, runner_location: Address) {
    use crate::compiler;
    use crate::stack::DoraToNativeInfo;

    let vm = get_vm();
    let _thread_handle: Handle<ManagedThread> = Handle::from_address(thread_location);
    let runner_handle: Handle<Obj> = Handle::from_address(runner_location);

    thread.tld.set_managed_thread_handle(thread_location);

    let stack_top = stack_pointer();
    let stack_limit = stack_top.sub(STACK_SIZE);
    thread.tld.set_stack_limit(stack_limit);

    // Thread was created in Parked state, so we need to Unpark
    // before we dereference handle.
    thread.unpark(vm);

    let vtable = runner_handle.header().vtbl();
    let class_instance = vtable.class_instance();

    let (lambda_id, type_params) = match &class_instance.kind {
        ShapeKind::Lambda(lambda_id, type_params) => (*lambda_id, type_params.clone()),
        _ => unreachable!(),
    };

    let tld = thread.tld_address();

    let fct_ptr = {
        let mut dtn = DoraToNativeInfo::new();

        thread.use_dtn(&mut dtn, || compiler::generate(vm, lambda_id, &type_params))
    };

    // execute the runner/lambda
    let dora_stub_address = vm.stubs.dora_entry();
    let fct: extern "C" fn(Address, Address, Ref<Obj>) =
        unsafe { mem::transmute(dora_stub_address) };
    fct(tld, fct_ptr, runner_handle.direct());

    // remove thread from list of all threads
    vm.threads.remove_current_thread();

    // notify threads waiting in join() for this thread's end
    thread.stop();
}

pub extern "C" fn join_thread(managed_thread: Handle<ManagedThread>) {
    let native_thread = managed_thread.native_thread();
    native_thread.join();
}

pub extern "C" fn mutex_wait(mutex: Handle<ManagedMutex>, value: i32) {
    let vm = get_vm();
    vm.wait_lists.block(mutex, value);
}

pub extern "C" fn mutex_notify(mutex: Handle<ManagedMutex>) {
    let vm = get_vm();
    vm.wait_lists.wakeup(mutex.direct_ptr());
}

pub extern "C" fn condition_enqueue(cond: Handle<ManagedCondition>) {
    let vm = get_vm();
    vm.wait_lists.enqueue(cond);
}

pub extern "C" fn condition_block_after_enqueue(_cond: Handle<Obj>) {
    let thread = current_thread();
    thread.block();
}

pub extern "C" fn condition_wakeup_one(cond: Handle<Obj>) {
    let vm = get_vm();
    vm.wait_lists.wakeup(cond.direct_ptr());
}

pub extern "C" fn condition_wakeup_all(cond: Handle<Obj>) {
    let vm = get_vm();
    vm.wait_lists.wakeup_all(cond.direct_ptr());
}
