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
    DoraThread, ManagedThread, STACK_SIZE, ThreadState, current_thread, deinit_current_thread,
    init_current_thread,
};
use crate::vm::{
    FctImplementation, Intrinsic, ManagedCondition, ManagedMutex, Trap, get_vm, stack_pointer,
};

use FctImplementation::Intrinsic as I;
use FctImplementation::Native as N;

pub const STDLIB_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    ("std::abort", N),
    ("std::exit", N),
    ("std::fatal_error", N),
    ("std::io::print", N),
    ("std::io::println", N),
    ("std::argc", N),
    ("std::argv", N),
    ("std::force_collect", N),
    ("std::force_minor_collect", N),
    ("std::timestamp", N),
    ("std::sleep", N),
    ("std::symbolize_stacktrace_element", N),
    ("std::thread::spawn", N),
    ("std::unreachable", I(Intrinsic::Unreachable)),
    ("std::fatal_error", I(Intrinsic::FatalError)),
    ("std::assert", I(Intrinsic::Assert)),
    ("std::debug", I(Intrinsic::Debug)),
    ("std::unsafe_kill_refs", I(Intrinsic::UnsafeKillRefs)),
    ("std::thread::Mutex#wait", N),
    ("std::thread::Mutex#notify", N),
    ("std::thread::Condition#enqueue", N),
    ("std::thread::Condition#block", N),
    ("std::thread::Condition#wakeup_one", N),
    ("std::thread::Condition#wakeup_all", N),
    ("std::thread::Thread#join", N),
    ("std::Stacktrace#capture", N),
    ("std::take_heap_snapshot", N),
    ("std::take_heap_snapshot_for_testing", N),
    // Bool
    (
        "std::traits::Equals for std::primitives::Bool#equals",
        I(Intrinsic::BoolEq),
    ),
    (
        "std::traits::Not for std::primitives::Bool#not",
        I(Intrinsic::BoolNot),
    ),
    ("std::primitives::Bool#to_int32", I(Intrinsic::BoolToInt32)),
    ("std::primitives::Bool#to_int64", I(Intrinsic::BoolToInt64)),
    // UInt8
    (
        "std::traits::Equals for std::primitives::UInt8#equals",
        I(Intrinsic::UInt8Eq),
    ),
    (
        "std::traits::Comparable for std::primitives::UInt8#cmp",
        I(Intrinsic::UInt8Cmp),
    ),
    (
        "std::string::Stringable for std::primitives::UInt8#to_string",
        N,
    ),
    ("std::primitives::UInt8#to_char", I(Intrinsic::UInt8ToChar)),
    (
        "std::primitives::UInt8#to_int32",
        I(Intrinsic::UInt8ToInt32),
    ),
    (
        "std::primitives::UInt8#to_int64",
        I(Intrinsic::UInt8ToInt64),
    ),
    // Char
    (
        "std::traits::Equals for std::primitives::Char#equals",
        I(Intrinsic::CharEq),
    ),
    (
        "std::traits::Comparable for std::primitives::Char#cmp",
        I(Intrinsic::CharCmp),
    ),
    ("std::primitives::Char#to_int32", I(Intrinsic::CharToInt32)),
    ("std::primitives::Char#to_int64", I(Intrinsic::CharToInt64)),
    (
        "std::string::Stringable for std::primitives::Char#to_string",
        N,
    ),
    // Int32
    (
        "std::traits::Equals for std::primitives::Int32#equals",
        I(Intrinsic::Int32Eq),
    ),
    (
        "std::traits::Comparable for std::primitives::Int32#cmp",
        I(Intrinsic::Int32Cmp),
    ),
    (
        "std::traits::Add for std::primitives::Int32#add",
        I(Intrinsic::Int32CheckedAdd),
    ),
    (
        "std::traits::Sub for std::primitives::Int32#sub",
        I(Intrinsic::Int32CheckedSub),
    ),
    (
        "std::traits::Mul for std::primitives::Int32#mul",
        I(Intrinsic::Int32CheckedMul),
    ),
    (
        "std::traits::Div for std::primitives::Int32#div",
        I(Intrinsic::Int32CheckedDiv),
    ),
    (
        "std::traits::Mod for std::primitives::Int32#modulo",
        I(Intrinsic::Int32CheckedMod),
    ),
    (
        "std::traits::BitOr for std::primitives::Int32#bitor",
        I(Intrinsic::Int32Or),
    ),
    (
        "std::traits::BitAnd for std::primitives::Int32#bitand",
        I(Intrinsic::Int32And),
    ),
    (
        "std::traits::BitXor for std::primitives::Int32#bitxor",
        I(Intrinsic::Int32Xor),
    ),
    (
        "std::traits::Shl for std::primitives::Int32#shl",
        I(Intrinsic::Int32Shl),
    ),
    (
        "std::traits::Shr for std::primitives::Int32#shr",
        I(Intrinsic::Int32Shr),
    ),
    (
        "std::traits::Sar for std::primitives::Int32#sar",
        I(Intrinsic::Int32Sar),
    ),
    (
        "std::traits::Not for std::primitives::Int32#not",
        I(Intrinsic::Int32Not),
    ),
    (
        "std::traits::Neg for std::primitives::Int32#neg",
        I(Intrinsic::Int32CheckedNeg),
    ),
    (
        "std::string::Stringable for std::primitives::Int32#to_string",
        N,
    ),
    (
        "std::primitives::Int32#wrapping_neg",
        I(Intrinsic::Int32WrappingNeg),
    ),
    (
        "std::primitives::Int32#overflowing_neg",
        I(Intrinsic::Int32OverflowingNeg),
    ),
    (
        "std::primitives::Int32#count_zero_bits_leading",
        I(Intrinsic::Int32CountZeroBitsLeading),
    ),
    (
        "std::primitives::Int32#count_zero_bits_trailing",
        I(Intrinsic::Int32CountZeroBitsTrailing),
    ),
    (
        "std::primitives::Int32#count_one_bits_leading",
        I(Intrinsic::Int32CountOneBitsLeading),
    ),
    (
        "std::primitives::Int32#count_one_bits_trailing",
        I(Intrinsic::Int32CountOneBitsTrailing),
    ),
    (
        "std::primitives::Int32#count_zero_bits",
        I(Intrinsic::Int32CountZeroBits),
    ),
    (
        "std::primitives::Int32#count_one_bits",
        I(Intrinsic::Int32CountOneBits),
    ),
    (
        "std::primitives::Int32#rotate_left",
        I(Intrinsic::Int32RotateLeft),
    ),
    (
        "std::primitives::Int32#rotate_right",
        I(Intrinsic::Int32RotateRight),
    ),
    (
        "std::primitives::Int32#wrapping_add",
        I(Intrinsic::Int32WrappingAdd),
    ),
    (
        "std::primitives::Int32#wrapping_sub",
        I(Intrinsic::Int32WrappingSub),
    ),
    (
        "std::primitives::Int32#wrapping_mul",
        I(Intrinsic::Int32WrappingMul),
    ),
    (
        "std::primitives::Int32#overflowing_add",
        I(Intrinsic::Int32OverflowingAdd),
    ),
    (
        "std::primitives::Int32#overflowing_sub",
        I(Intrinsic::Int32OverflowingSub),
    ),
    (
        "std::primitives::Int32#overflowing_mul",
        I(Intrinsic::Int32OverflowingMul),
    ),
    (
        "std::primitives::Int32#overflowing_div",
        I(Intrinsic::Int32OverflowingDiv),
    ),
    (
        "std::primitives::Int32#overflowing_mod",
        I(Intrinsic::Int32OverflowingMod),
    ),
    (
        "std::primitives::Int32#to_uint8",
        I(Intrinsic::Int32ToUInt8),
    ),
    (
        "std::primitives::Int32#to_char_unchecked",
        I(Intrinsic::Int32ToCharUnchecked),
    ),
    (
        "std::primitives::Int32#to_int64",
        I(Intrinsic::Int32ToInt64),
    ),
    (
        "std::primitives::Int32#to_float32",
        I(Intrinsic::Int32ToFloat32),
    ),
    (
        "std::primitives::Int32#to_float64",
        I(Intrinsic::Int32ToFloat64),
    ),
    (
        "std::primitives::Int32#as_float32",
        I(Intrinsic::ReinterpretInt32AsFloat32),
    ),
    // Int64
    (
        "std::traits::Equals for std::primitives::Int64#equals",
        I(Intrinsic::Int64Eq),
    ),
    (
        "std::traits::Comparable for std::primitives::Int64#cmp",
        I(Intrinsic::Int64Cmp),
    ),
    (
        "std::traits::Add for std::primitives::Int64#add",
        I(Intrinsic::Int64CheckedAdd),
    ),
    (
        "std::traits::Sub for std::primitives::Int64#sub",
        I(Intrinsic::Int64CheckedSub),
    ),
    (
        "std::traits::Mul for std::primitives::Int64#mul",
        I(Intrinsic::Int64CheckedMul),
    ),
    (
        "std::traits::Div for std::primitives::Int64#div",
        I(Intrinsic::Int64CheckedDiv),
    ),
    (
        "std::traits::Mod for std::primitives::Int64#modulo",
        I(Intrinsic::Int64CheckedMod),
    ),
    (
        "std::traits::BitOr for std::primitives::Int64#bitor",
        I(Intrinsic::Int64Or),
    ),
    (
        "std::traits::BitAnd for std::primitives::Int64#bitand",
        I(Intrinsic::Int64And),
    ),
    (
        "std::traits::BitXor for std::primitives::Int64#bitxor",
        I(Intrinsic::Int64Xor),
    ),
    (
        "std::traits::Shl for std::primitives::Int64#shl",
        I(Intrinsic::Int64Shl),
    ),
    (
        "std::traits::Shr for std::primitives::Int64#shr",
        I(Intrinsic::Int64Shr),
    ),
    (
        "std::traits::Sar for std::primitives::Int64#sar",
        I(Intrinsic::Int64Sar),
    ),
    (
        "std::traits::Not for std::primitives::Int64#not",
        I(Intrinsic::Int64Not),
    ),
    (
        "std::traits::Neg for std::primitives::Int64#neg",
        I(Intrinsic::Int64CheckedNeg),
    ),
    (
        "std::string::Stringable for std::primitives::Int64#to_string",
        N,
    ),
    (
        "std::primitives::Int64#wrapping_neg",
        I(Intrinsic::Int64WrappingNeg),
    ),
    (
        "std::primitives::Int64#overflowing_neg",
        I(Intrinsic::Int64OverflowingNeg),
    ),
    (
        "std::primitives::Int64#count_zero_bits_leading",
        I(Intrinsic::Int64CountZeroBitsLeading),
    ),
    (
        "std::primitives::Int64#count_zero_bits_trailing",
        I(Intrinsic::Int64CountZeroBitsTrailing),
    ),
    (
        "std::primitives::Int64#count_one_bits_leading",
        I(Intrinsic::Int64CountOneBitsLeading),
    ),
    (
        "std::primitives::Int64#count_one_bits_trailing",
        I(Intrinsic::Int64CountOneBitsTrailing),
    ),
    (
        "std::primitives::Int64#count_zero_bits",
        I(Intrinsic::Int64CountZeroBits),
    ),
    (
        "std::primitives::Int64#count_one_bits",
        I(Intrinsic::Int64CountOneBits),
    ),
    (
        "std::primitives::Int64#rotate_left",
        I(Intrinsic::Int64RotateLeft),
    ),
    (
        "std::primitives::Int64#rotate_right",
        I(Intrinsic::Int64RotateRight),
    ),
    (
        "std::primitives::Int64#wrapping_add",
        I(Intrinsic::Int64WrappingAdd),
    ),
    (
        "std::primitives::Int64#wrapping_sub",
        I(Intrinsic::Int64WrappingSub),
    ),
    (
        "std::primitives::Int64#wrapping_mul",
        I(Intrinsic::Int64WrappingMul),
    ),
    (
        "std::primitives::Int64#overflowing_add",
        I(Intrinsic::Int64OverflowingAdd),
    ),
    (
        "std::primitives::Int64#overflowing_sub",
        I(Intrinsic::Int64OverflowingSub),
    ),
    (
        "std::primitives::Int64#overflowing_mul",
        I(Intrinsic::Int64OverflowingMul),
    ),
    (
        "std::primitives::Int64#overflowing_div",
        I(Intrinsic::Int64OverflowingDiv),
    ),
    (
        "std::primitives::Int64#overflowing_mod",
        I(Intrinsic::Int64OverflowingMod),
    ),
    (
        "std::primitives::Int64#to_uint8",
        I(Intrinsic::Int64ToUInt8),
    ),
    (
        "std::primitives::Int64#to_int32",
        I(Intrinsic::Int64ToInt32),
    ),
    (
        "std::primitives::Int64#to_char_unchecked",
        I(Intrinsic::Int64ToCharUnchecked),
    ),
    (
        "std::primitives::Int64#to_float32",
        I(Intrinsic::Int64ToFloat32),
    ),
    (
        "std::primitives::Int64#to_float64",
        I(Intrinsic::Int64ToFloat64),
    ),
    (
        "std::primitives::Int64#as_float64",
        I(Intrinsic::ReinterpretInt64AsFloat64),
    ),
    // Float32
    (
        "std::traits::Equals for std::primitives::Float32#equals",
        I(Intrinsic::Float32Eq),
    ),
    (
        "std::traits::Comparable for std::primitives::Float32#cmp",
        I(Intrinsic::Float32Cmp),
    ),
    (
        "std::traits::Add for std::primitives::Float32#add",
        I(Intrinsic::Float32Add),
    ),
    (
        "std::traits::Sub for std::primitives::Float32#sub",
        I(Intrinsic::Float32Sub),
    ),
    (
        "std::traits::Mul for std::primitives::Float32#mul",
        I(Intrinsic::Float32Mul),
    ),
    (
        "std::traits::Div for std::primitives::Float32#div",
        I(Intrinsic::Float32Div),
    ),
    (
        "std::traits::Neg for std::primitives::Float32#neg",
        I(Intrinsic::Float32Neg),
    ),
    ("std::primitives::Float32#abs", I(Intrinsic::Float32Abs)),
    (
        "std::primitives::Float32#is_nan",
        I(Intrinsic::Float32IsNan),
    ),
    (
        "std::primitives::Float32#round_to_zero",
        I(Intrinsic::Float32RoundToZero),
    ),
    (
        "std::primitives::Float32#round_up",
        I(Intrinsic::Float32RoundUp),
    ),
    (
        "std::primitives::Float32#round_down",
        I(Intrinsic::Float32RoundDown),
    ),
    (
        "std::primitives::Float32#round_half_even",
        I(Intrinsic::Float32RoundHalfEven),
    ),
    ("std::primitives::Float32#sqrt", I(Intrinsic::Float32Sqrt)),
    (
        "std::primitives::Float32#to_int32",
        I(Intrinsic::Float32ToInt32),
    ),
    (
        "std::primitives::Float32#to_int64",
        I(Intrinsic::Float32ToInt64),
    ),
    (
        "std::primitives::Float32#to_float64",
        I(Intrinsic::PromoteFloat32ToFloat64),
    ),
    (
        "std::primitives::Float32#as_int32",
        I(Intrinsic::ReinterpretFloat32AsInt32),
    ),
    // Float64
    (
        "std::traits::Equals for std::primitives::Float64#equals",
        I(Intrinsic::Float64Eq),
    ),
    (
        "std::traits::Comparable for std::primitives::Float64#cmp",
        I(Intrinsic::Float64Cmp),
    ),
    (
        "std::traits::Add for std::primitives::Float64#add",
        I(Intrinsic::Float64Add),
    ),
    (
        "std::traits::Sub for std::primitives::Float64#sub",
        I(Intrinsic::Float64Sub),
    ),
    (
        "std::traits::Mul for std::primitives::Float64#mul",
        I(Intrinsic::Float64Mul),
    ),
    (
        "std::traits::Div for std::primitives::Float64#div",
        I(Intrinsic::Float64Div),
    ),
    (
        "std::traits::Neg for std::primitives::Float64#neg",
        I(Intrinsic::Float64Neg),
    ),
    ("std::primitives::Float64#abs", I(Intrinsic::Float64Abs)),
    (
        "std::primitives::Float64#is_nan",
        I(Intrinsic::Float64IsNan),
    ),
    (
        "std::primitives::Float64#round_to_zero",
        I(Intrinsic::Float64RoundToZero),
    ),
    (
        "std::primitives::Float64#round_up",
        I(Intrinsic::Float64RoundUp),
    ),
    (
        "std::primitives::Float64#round_down",
        I(Intrinsic::Float64RoundDown),
    ),
    (
        "std::primitives::Float64#round_half_even",
        I(Intrinsic::Float64RoundHalfEven),
    ),
    ("std::primitives::Float64#sqrt", I(Intrinsic::Float64Sqrt)),
    (
        "std::primitives::Float64#to_int32",
        I(Intrinsic::Float64ToInt32),
    ),
    (
        "std::primitives::Float64#to_int64",
        I(Intrinsic::Float64ToInt64),
    ),
    (
        "std::primitives::Float64#to_float32",
        I(Intrinsic::DemoteFloat64ToFloat32),
    ),
    (
        "std::primitives::Float64#as_int64",
        I(Intrinsic::ReinterpretFloat64AsInt64),
    ),
    // String
    ("std::traits::Add for std::string::String#add", N),
    ("std::string::String#clone", N),
    ("std::string::String#from_bytes_part", N),
    ("std::string::String#from_string_part", N),
    ("std::string::String#compare_to", N),
    ("std::string::String#to_int32_success", N),
    ("std::string::String#to_int64_success", N),
    ("std::string::String#to_int32_or_zero", N),
    ("std::string::String#to_int64_or_zero", N),
    ("std::string::String#to_float32_success", N),
    ("std::string::String#to_float64_success", N),
    ("std::string::String#to_float32_or_zero", N),
    ("std::string::String#to_float64_or_zero", N),
    ("std::string::String#size", I(Intrinsic::StrLen)),
    ("std::string::String#get_byte", I(Intrinsic::StrGet)),
    // Array
    ("std::collections::Array#size", I(Intrinsic::ArrayLen)),
    (
        "std::traits::IndexGet for std::collections::Array#get",
        I(Intrinsic::ArrayGet),
    ),
    (
        "std::traits::IndexSet for std::collections::Array#set",
        I(Intrinsic::ArraySet),
    ),
    (
        "std::collections::Array#unsafe_new",
        I(Intrinsic::ArrayNewOfSize),
    ),
    ("std::collections::Array#new", I(Intrinsic::ArrayWithValues)),
    // Option
    (
        "std::primitives::Option#get_or_panic",
        I(Intrinsic::OptionGetOrPanic),
    ),
    (
        "std::primitives::Option#is_none",
        I(Intrinsic::OptionIsNone),
    ),
    (
        "std::primitives::Option#is_some",
        I(Intrinsic::OptionIsSome),
    ),
    // AtomicInt32
    ("std::thread::AtomicInt32#get", I(Intrinsic::AtomicInt32Get)),
    ("std::thread::AtomicInt32#set", I(Intrinsic::AtomicInt32Set)),
    (
        "std::thread::AtomicInt32#exchange",
        I(Intrinsic::AtomicInt32Exchange),
    ),
    (
        "std::thread::AtomicInt32#compare_exchange",
        I(Intrinsic::AtomicInt32CompareExchange),
    ),
    (
        "std::thread::AtomicInt32#fetch_add",
        I(Intrinsic::AtomicInt32FetchAdd),
    ),
    // AtomicInt64
    ("std::thread::AtomicInt64#get", I(Intrinsic::AtomicInt64Get)),
    ("std::thread::AtomicInt64#set", I(Intrinsic::AtomicInt64Set)),
    (
        "std::thread::AtomicInt64#exchange",
        I(Intrinsic::AtomicInt64Exchange),
    ),
    (
        "std::thread::AtomicInt64#compare_exchange",
        I(Intrinsic::AtomicInt64CompareExchange),
    ),
    (
        "std::thread::AtomicInt64#fetch_add",
        I(Intrinsic::AtomicInt64FetchAdd),
    ),
    // Thread
    ("std::thread::Thread#current", I(Intrinsic::ThreadCurrent)),
    (
        "std::string::Stringable for std::primitives::Float32#to_string",
        N,
    ),
    (
        "std::string::Stringable for std::primitives::Float64#to_string",
        N,
    ),
];

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
    let dora_stub_address = vm.native_methods.dora_entry_trampoline();
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
