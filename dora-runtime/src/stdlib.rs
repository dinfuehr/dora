use libc;

use dora_runtime_macros::dora_native;

use std::char;
use std::fs::File;
use std::io::Write;
use std::mem;
use std::str;
use std::thread;
use std::time::Duration;

use crate::gc::{Address, GcReason};
use crate::handle::{Handle, create_handle, handle_scope};
use crate::mirror::{Object, Ref, Str, UInt8Array};
use crate::stack::stacktrace_from_last_dtn;
use crate::threads::{
    DoraThread, ManagedThread, STACK_SIZE, current_thread, deinit_current_thread,
    init_current_thread,
};
use crate::vm::{ManagedCondition, ManagedMutex, Trap, get_vm, stack_pointer};
use dora_compiler::ThreadState;

pub mod io;

#[dora_native("std::string::Stringable for std::primitives::UInt8#to_string")]
pub extern "C" fn uint8_to_string(val: u8) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::string::Stringable for std::primitives::Char#to_string")]
pub extern "C" fn char_to_string(val: u32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = unsafe { char::from_u32_unchecked(val) }.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::string::Stringable for std::primitives::Int32#to_string")]
pub extern "C" fn int32_to_string(val: i32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::string::Stringable for std::primitives::Int64#to_string")]
pub extern "C" fn int64_to_string(val: i64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::string::Stringable for std::primitives::Float32#to_string")]
pub extern "C" fn float32_to_string(val: f32) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::string::Stringable for std::primitives::Float64#to_string")]
pub extern "C" fn float64_to_string(val: f64) -> Ref<Str> {
    handle_scope(|| {
        let buffer = val.to_string();
        let vm = get_vm();

        Str::from_buffer(vm, buffer.as_bytes())
    })
}

#[dora_native("std::io::print")]
pub extern "C" fn print(val: Handle<Str>) {
    std::io::stdout().write(val.content()).unwrap();
}

#[dora_native("std::fatal_error")]
pub extern "C" fn fatal_error(msg: Handle<Str>) {
    eprint!("fatal error: ");
    std::io::stderr().write(msg.content()).unwrap();
    eprintln!("");

    let vm = get_vm();
    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump_to_stderr(vm);

    std::process::exit(1);
}

#[dora_native("std::abort")]
extern "C" fn abort() {
    eprintln!("program aborted.");
    std::process::exit(1);
}

#[dora_native("std::exit")]
extern "C" fn exit(status: i32) {
    std::process::exit(status);
}

#[unsafe(export_name = "dora_native_unreachable")]
pub extern "C" fn unreachable() {
    let vm = get_vm();

    eprintln!("unreachable code executed.");

    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump_to_stderr(vm);

    std::process::exit(1);
}

#[dora_native("std::timestamp")]
extern "C" fn timestamp() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let timestamp = SystemTime::now();
    timestamp.duration_since(UNIX_EPOCH).unwrap().as_millis() as u64
}

#[dora_native("std::io::println")]
extern "C" fn println(val: Handle<Str>) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    handle.write(val.content()).unwrap();
    handle.write(b"\n").unwrap();
}

#[dora_native("std::sleep")]
extern "C" fn sleep(seconds: i32) {
    assert!(seconds >= 0);
    thread::sleep(Duration::from_secs(seconds as u64));
}

#[dora_native("std::string::String#compare_to")]
pub extern "C" fn strcmp(lhs: Handle<Str>, rhs: Handle<Str>) -> i32 {
    unsafe {
        libc::strcmp(
            lhs.data() as *const libc::c_char,
            rhs.data() as *const libc::c_char,
        )
    }
}

#[dora_native("std::traits::Add for std::string::String#add")]
pub extern "C" fn strcat(lhs: Handle<Str>, rhs: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();
        Str::concat(vm, lhs, rhs).direct()
    })
}

#[dora_native("std::string::String#clone")]
pub extern "C" fn str_clone(val: Handle<Str>) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();

        val.dup(vm)
    })
}

#[dora_native("std::string::String#from_bytes_part")]
pub extern "C" fn str_from_bytes(val: Handle<UInt8Array>, offset: usize, len: usize) -> Ref<Str> {
    str_from_str(val.cast(), offset, len)
}

#[dora_native("std::string::String#from_string_part")]
pub extern "C" fn str_from_string(val: Handle<Str>, offset: usize, len: usize) -> Ref<Str> {
    str_from_str(val, offset, len)
}

fn str_from_str(val: Handle<Str>, offset: usize, len: usize) -> Ref<Str> {
    handle_scope(|| {
        let vm = get_vm();

        Str::from_str(vm, val, offset, len)
    })
}

#[unsafe(export_name = "dora_native_gc_alloc")]
pub extern "C" fn gc_alloc(size: usize) -> *mut Object {
    let vm = get_vm();
    vm.gc.alloc(vm, size).to_mut_ptr()
}

#[dora_native("std::force_collect")]
extern "C" fn gc_collect() {
    let vm = get_vm();
    vm.gc.force_collect(vm, GcReason::ForceCollect);
}

#[dora_native("std::force_minor_collect")]
extern "C" fn gc_minor_collect() {
    let vm = get_vm();
    vm.gc.force_collect(vm, GcReason::ForceMinorCollect);
}

#[dora_native("std::argc")]
extern "C" fn argc() -> i32 {
    let vm = get_vm();

    vm.program_args.len() as i32
}

#[dora_native("std::argv")]
extern "C" fn argv(ind: i32) -> Ref<Str> {
    let vm = get_vm();

    if ind >= 0 && (ind as usize) < vm.program_args.len() {
        let value = &vm.program_args[ind as usize];

        return Str::from_buffer(vm, value.as_bytes());
    }

    panic!("argument does not exist");
}

#[dora_native("std::string::String#to_int32_success")]
pub extern "C" fn str_to_int32_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().is_ok()
}

#[dora_native("std::string::String#to_int32_or_zero")]
pub extern "C" fn str_to_int32(val: Handle<Str>) -> i32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i32>().unwrap_or(0)
}

#[dora_native("std::string::String#to_int64_success")]
pub extern "C" fn str_to_int64_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().is_ok()
}

#[dora_native("std::string::String#to_int64_or_zero")]
pub extern "C" fn str_to_int64(val: Handle<Str>) -> i64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<i64>().unwrap_or(0)
}

#[dora_native("std::string::String#to_float32_success")]
pub extern "C" fn str_to_float32_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f32>().is_ok()
}

#[dora_native("std::string::String#to_float32_or_zero")]
pub extern "C" fn str_to_float32(val: Handle<Str>) -> f32 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f32>().unwrap_or(0.0f32)
}

#[dora_native("std::string::String#to_float64_success")]
pub extern "C" fn str_to_float64_success(val: Handle<Str>) -> bool {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f64>().is_ok()
}

#[dora_native("std::string::String#to_float64_or_zero")]
pub extern "C" fn str_to_float64(val: Handle<Str>) -> f64 {
    let slice = val.content();
    let val = str::from_utf8(slice).unwrap();

    val.parse::<f64>().unwrap_or(0.0)
}

#[unsafe(export_name = "dora_native_trap")]
pub extern "C" fn trap(trap_id: u32) {
    let vm = get_vm();
    let trap = Trap::try_from(trap_id as u8).expect("illegal trap code");

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
        Trap::SHIFT => "shift amount out of bounds",
    };

    eprintln!("{}", msg);
    let stacktrace = stacktrace_from_last_dtn(vm);
    stacktrace.dump_to_stderr(vm);
    unsafe {
        libc::_exit(101 + trap_id as i32);
    }
}

pub extern "C" fn stack_overflow() {
    trap(Trap::STACK_OVERFLOW as u32);
}

#[dora_native("std::thread::spawn")]
pub extern "C" fn spawn_thread(runner: Handle<Object>) -> Address {
    let vm = get_vm();

    handle_scope(|| {
        let managed_thread = ManagedThread::alloc(vm);
        let mut managed_thread: Handle<ManagedThread> = create_handle(managed_thread);

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
        let thread_location = thread
            .handles
            .create_handle(managed_thread.direct())
            .location();
        let runner_location = thread.handles.create_handle(runner.direct()).location();

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
    let vm = get_vm();
    let _thread_handle: Handle<ManagedThread> = Handle::from_address(thread_location);
    let runner_handle: Handle<Object> = Handle::from_address(runner_location);

    thread.tld.set_managed_thread_handle(thread_location);

    let stack_top = stack_pointer();
    let stack_limit = stack_top.sub(STACK_SIZE);
    thread.tld.set_stack_limit(stack_limit);

    // Thread was created in Parked state, so we need to Unpark
    // before we dereference handle.
    thread.unpark(vm);

    let shape = runner_handle.header().shape(vm.meta_space_start());

    let fct_ptr = shape
        .table()
        .first()
        .copied()
        .expect("missing lambda vtable entry");
    let fct_ptr = Address::from(fct_ptr);

    let tld = thread.tld_address();

    // execute the runner/lambda
    let dora_stub_address = vm
        .dora_entry_trampoline
        .expect("uninitialized dora_entry_trampoline");
    let fct: extern "C" fn(Address, Address, Ref<Object>) =
        unsafe { mem::transmute(dora_stub_address) };
    fct(tld, fct_ptr, runner_handle.direct());

    // remove thread from list of all threads
    vm.threads.remove_current_thread();

    // notify threads waiting in join() for this thread's end
    thread.stop();
}

#[dora_native("std::thread::Thread#join")]
pub extern "C" fn join_thread(managed_thread: Handle<ManagedThread>) {
    let native_thread = managed_thread.native_thread();
    native_thread.join();
}

#[dora_native("std::thread::Mutex#wait")]
pub extern "C" fn mutex_wait(mutex: Handle<ManagedMutex>, value: i32) {
    let vm = get_vm();
    vm.wait_lists.block(mutex, value);
}

#[dora_native("std::thread::Mutex#notify")]
pub extern "C" fn mutex_notify(mutex: Handle<ManagedMutex>) {
    let vm = get_vm();
    vm.wait_lists.wakeup(mutex.direct_ptr());
}

#[dora_native("std::thread::Condition#enqueue")]
pub extern "C" fn condition_enqueue(cond: Handle<ManagedCondition>) {
    let vm = get_vm();
    vm.wait_lists.enqueue(cond);
}

#[dora_native("std::thread::Condition#block")]
pub extern "C" fn condition_block_after_enqueue(_cond: Handle<Object>) {
    let thread = current_thread();
    thread.block();
}

#[dora_native("std::thread::Condition#wakeup_one")]
pub extern "C" fn condition_wakeup_one(cond: Handle<Object>) {
    let vm = get_vm();
    vm.wait_lists.wakeup(cond.direct_ptr());
}

#[dora_native("std::thread::Condition#wakeup_all")]
pub extern "C" fn condition_wakeup_all(cond: Handle<Object>) {
    let vm = get_vm();
    vm.wait_lists.wakeup_all(cond.direct_ptr());
}

#[dora_native("std::take_heap_snapshot")]
pub extern "C" fn take_heap_snapshot() {
    use crate::snapshot::SnapshotGenerator;

    let vm = get_vm();
    let file = File::create("dora.heapsnapshot").expect("Failed to create file");
    let snapshot = SnapshotGenerator::new(vm, file).unwrap();
    snapshot
        .generate_in_safepoint()
        .expect("Failed to generate snapshot");
}

#[dora_native("std::take_heap_snapshot_for_testing")]
pub extern "C" fn take_heap_snapshot_for_testing() {
    use crate::snapshot::SnapshotGenerator;

    let vm = get_vm();
    let file = tempfile::tempfile().expect("Failed to open temporary file.");
    let snapshot = SnapshotGenerator::new(vm, file).unwrap();
    snapshot
        .generate_in_safepoint()
        .expect("Failed to generate snapshot");
}
