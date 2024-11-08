use libc;

use std::char;
use std::io::Write;
use std::mem;
use std::str;
use std::thread;
use std::time::Duration;

use crate::gc::{Address, GcReason};
use crate::handle::{create_handle, handle_scope, Handle};
use crate::mirror::{Object, Ref, Str, UInt8Array};
use crate::stack;
use crate::stack::stacktrace_from_last_dtn;
use crate::stdlib;
use crate::threads::{
    current_thread, deinit_current_thread, init_current_thread, DoraThread, ManagedThread,
    ThreadState, STACK_SIZE,
};
use crate::vm::{
    get_vm, stack_pointer, FctImplementation, Intrinsic, ManagedCondition, ManagedMutex, ShapeKind,
    Trap,
};

use FctImplementation::Intrinsic as I;
use FctImplementation::Native as N;

pub const STDLIB_FUNCTIONS: &[(&'static str, FctImplementation)] = &[
    ("stdlib::abort", N(abort as *const u8)),
    ("stdlib::exit", N(exit as *const u8)),
    ("stdlib::fatalError", N(fatal_error as *const u8)),
    ("stdlib::print", N(print as *const u8)),
    ("stdlib::println", N(println as *const u8)),
    ("stdlib::argc", N(argc as *const u8)),
    ("stdlib::argv", N(argv as *const u8)),
    ("stdlib::forceCollect", N(gc_collect as *const u8)),
    (
        "stdlib::forceMinorCollect",
        N(gc_minor_collect as *const u8),
    ),
    ("stdlib::timestamp", N(timestamp as *const u8)),
    ("stdlib::sleep", N(sleep as *const u8)),
    (
        "stdlib::symbolizeStacktraceElement",
        N(stack::symbolize_stack_trace_element as *const u8),
    ),
    (
        "stdlib::thread::spawn",
        N(stdlib::spawn_thread as *const u8),
    ),
    ("stdlib::unreachable", I(Intrinsic::Unreachable)),
    ("stdlib::fatalError", I(Intrinsic::FatalError)),
    ("stdlib::assert", I(Intrinsic::Assert)),
    ("stdlib::debug", I(Intrinsic::Debug)),
    ("stdlib::unsafeKillRefs", I(Intrinsic::UnsafeKillRefs)),
    (
        "stdlib::thread::Mutex#wait",
        N(stdlib::mutex_wait as *const u8),
    ),
    (
        "stdlib::thread::Mutex#notify",
        N(stdlib::mutex_notify as *const u8),
    ),
    (
        "stdlib::thread::Condition#enqueue",
        N(stdlib::condition_enqueue as *const u8),
    ),
    (
        "stdlib::thread::Condition#block",
        N(stdlib::condition_block_after_enqueue as *const u8),
    ),
    (
        "stdlib::thread::Condition#wakeupOne",
        N(stdlib::condition_wakeup_one as *const u8),
    ),
    (
        "stdlib::thread::Condition#wakeupAll",
        N(stdlib::condition_wakeup_all as *const u8),
    ),
    (
        "stdlib::thread::Thread#join",
        N(stdlib::join_thread as *const u8),
    ),
    (
        "stdlib::Stacktrace#capture",
        N(stack::capture_stack_trace as *const u8),
    ),
    // Bool
    (
        "stdlib::traits::Equals for stdlib::primitives::Bool#equals",
        I(Intrinsic::BoolEq),
    ),
    (
        "stdlib::traits::Not for stdlib::primitives::Bool#not",
        I(Intrinsic::BoolNot),
    ),
    (
        "stdlib::primitives::Bool#toInt32",
        I(Intrinsic::BoolToInt32),
    ),
    (
        "stdlib::primitives::Bool#toInt64",
        I(Intrinsic::BoolToInt64),
    ),
    // UInt8
    (
        "stdlib::traits::Equals for stdlib::primitives::UInt8#equals",
        I(Intrinsic::UInt8Eq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::UInt8#cmp",
        I(Intrinsic::UInt8Cmp),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::UInt8#toString",
        N(stdlib::uint8_to_string as *const u8),
    ),
    (
        "stdlib::primitives::UInt8#toChar",
        I(Intrinsic::UInt8ToChar),
    ),
    (
        "stdlib::primitives::UInt8#toInt32",
        I(Intrinsic::UInt8ToInt32),
    ),
    (
        "stdlib::primitives::UInt8#toInt64",
        I(Intrinsic::UInt8ToInt64),
    ),
    // Char
    (
        "stdlib::traits::Equals for stdlib::primitives::Char#equals",
        I(Intrinsic::CharEq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::Char#cmp",
        I(Intrinsic::CharCmp),
    ),
    (
        "stdlib::primitives::Char#toInt32",
        I(Intrinsic::CharToInt32),
    ),
    (
        "stdlib::primitives::Char#toInt64",
        I(Intrinsic::CharToInt64),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::Char#toString",
        N(stdlib::char_to_string as *const u8),
    ),
    // Int32
    (
        "stdlib::traits::Equals for stdlib::primitives::Int32#equals",
        I(Intrinsic::Int32Eq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::Int32#cmp",
        I(Intrinsic::Int32Cmp),
    ),
    (
        "stdlib::traits::Add for stdlib::primitives::Int32#add",
        I(Intrinsic::Int32Add),
    ),
    (
        "stdlib::traits::Sub for stdlib::primitives::Int32#sub",
        I(Intrinsic::Int32Sub),
    ),
    (
        "stdlib::traits::Mul for stdlib::primitives::Int32#mul",
        I(Intrinsic::Int32Mul),
    ),
    (
        "stdlib::traits::Div for stdlib::primitives::Int32#div",
        I(Intrinsic::Int32Div),
    ),
    (
        "stdlib::traits::Mod for stdlib::primitives::Int32#modulo",
        I(Intrinsic::Int32Mod),
    ),
    (
        "stdlib::traits::BitOr for stdlib::primitives::Int32#bitor",
        I(Intrinsic::Int32Or),
    ),
    (
        "stdlib::traits::BitAnd for stdlib::primitives::Int32#bitand",
        I(Intrinsic::Int32And),
    ),
    (
        "stdlib::traits::BitXor for stdlib::primitives::Int32#bitxor",
        I(Intrinsic::Int32Xor),
    ),
    (
        "stdlib::traits::Shl for stdlib::primitives::Int32#shl",
        I(Intrinsic::Int32Shl),
    ),
    (
        "stdlib::traits::Shr for stdlib::primitives::Int32#shr",
        I(Intrinsic::Int32Shr),
    ),
    (
        "stdlib::traits::Sar for stdlib::primitives::Int32#sar",
        I(Intrinsic::Int32Sar),
    ),
    (
        "stdlib::traits::Not for stdlib::primitives::Int32#not",
        I(Intrinsic::Int32Not),
    ),
    (
        "stdlib::traits::Neg for stdlib::primitives::Int32#neg",
        I(Intrinsic::Int32Neg),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::Int32#toString",
        N(stdlib::int32_to_string as *const u8),
    ),
    (
        "stdlib::primitives::Int32#wrappingNeg",
        I(Intrinsic::Int32NegUnchecked),
    ),
    (
        "stdlib::primitives::Int32#countZeroBitsLeading",
        I(Intrinsic::Int32CountZeroBitsLeading),
    ),
    (
        "stdlib::primitives::Int32#countZeroBitsTrailing",
        I(Intrinsic::Int32CountZeroBitsTrailing),
    ),
    (
        "stdlib::primitives::Int32#countOneBitsLeading",
        I(Intrinsic::Int32CountOneBitsLeading),
    ),
    (
        "stdlib::primitives::Int32#countOneBitsTrailing",
        I(Intrinsic::Int32CountOneBitsTrailing),
    ),
    (
        "stdlib::primitives::Int32#countZeroBits",
        I(Intrinsic::Int32CountZeroBits),
    ),
    (
        "stdlib::primitives::Int32#countOneBits",
        I(Intrinsic::Int32CountOneBits),
    ),
    (
        "stdlib::primitives::Int32#rotateLeft",
        I(Intrinsic::Int32RotateLeft),
    ),
    (
        "stdlib::primitives::Int32#rotateRight",
        I(Intrinsic::Int32RotateRight),
    ),
    (
        "stdlib::primitives::Int32#wrappingAdd",
        I(Intrinsic::Int32AddUnchecked),
    ),
    (
        "stdlib::primitives::Int32#wrappingSub",
        I(Intrinsic::Int32SubUnchecked),
    ),
    (
        "stdlib::primitives::Int32#wrappingMul",
        I(Intrinsic::Int32MulUnchecked),
    ),
    (
        "stdlib::primitives::Int32#toUInt8",
        I(Intrinsic::Int32ToUInt8),
    ),
    (
        "stdlib::primitives::Int32#toCharUnchecked",
        I(Intrinsic::Int32ToCharUnchecked),
    ),
    (
        "stdlib::primitives::Int32#toInt64",
        I(Intrinsic::Int32ToInt64),
    ),
    (
        "stdlib::primitives::Int32#toFloat32",
        I(Intrinsic::Int32ToFloat32),
    ),
    (
        "stdlib::primitives::Int32#toFloat64",
        I(Intrinsic::Int32ToFloat64),
    ),
    (
        "stdlib::primitives::Int32#asFloat32",
        I(Intrinsic::ReinterpretInt32AsFloat32),
    ),
    // Int64
    (
        "stdlib::traits::Equals for stdlib::primitives::Int64#equals",
        I(Intrinsic::Int64Eq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::Int64#cmp",
        I(Intrinsic::Int64Cmp),
    ),
    (
        "stdlib::traits::Add for stdlib::primitives::Int64#add",
        I(Intrinsic::Int64Add),
    ),
    (
        "stdlib::traits::Sub for stdlib::primitives::Int64#sub",
        I(Intrinsic::Int64Sub),
    ),
    (
        "stdlib::traits::Mul for stdlib::primitives::Int64#mul",
        I(Intrinsic::Int64Mul),
    ),
    (
        "stdlib::traits::Div for stdlib::primitives::Int64#div",
        I(Intrinsic::Int64Div),
    ),
    (
        "stdlib::traits::Mod for stdlib::primitives::Int64#modulo",
        I(Intrinsic::Int64Mod),
    ),
    (
        "stdlib::traits::BitOr for stdlib::primitives::Int64#bitor",
        I(Intrinsic::Int64Or),
    ),
    (
        "stdlib::traits::BitAnd for stdlib::primitives::Int64#bitand",
        I(Intrinsic::Int64And),
    ),
    (
        "stdlib::traits::BitXor for stdlib::primitives::Int64#bitxor",
        I(Intrinsic::Int64Xor),
    ),
    (
        "stdlib::traits::Shl for stdlib::primitives::Int64#shl",
        I(Intrinsic::Int64Shl),
    ),
    (
        "stdlib::traits::Shr for stdlib::primitives::Int64#shr",
        I(Intrinsic::Int64Shr),
    ),
    (
        "stdlib::traits::Sar for stdlib::primitives::Int64#sar",
        I(Intrinsic::Int64Sar),
    ),
    (
        "stdlib::traits::Not for stdlib::primitives::Int64#not",
        I(Intrinsic::Int64Not),
    ),
    (
        "stdlib::traits::Neg for stdlib::primitives::Int64#neg",
        I(Intrinsic::Int64Neg),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::Int64#toString",
        N(stdlib::int64_to_string as *const u8),
    ),
    (
        "stdlib::primitives::Int64#wrappingNeg",
        I(Intrinsic::Int64NegUnchecked),
    ),
    (
        "stdlib::primitives::Int64#countZeroBitsLeading",
        I(Intrinsic::Int64CountZeroBitsLeading),
    ),
    (
        "stdlib::primitives::Int64#countZeroBitsTrailing",
        I(Intrinsic::Int64CountZeroBitsTrailing),
    ),
    (
        "stdlib::primitives::Int64#countOneBitsLeading",
        I(Intrinsic::Int64CountOneBitsLeading),
    ),
    (
        "stdlib::primitives::Int64#countOneBitsTrailing",
        I(Intrinsic::Int64CountOneBitsTrailing),
    ),
    (
        "stdlib::primitives::Int64#countZeroBits",
        I(Intrinsic::Int64CountZeroBits),
    ),
    (
        "stdlib::primitives::Int64#countOneBits",
        I(Intrinsic::Int64CountOneBits),
    ),
    (
        "stdlib::primitives::Int64#rotateLeft",
        I(Intrinsic::Int64RotateLeft),
    ),
    (
        "stdlib::primitives::Int64#rotateRight",
        I(Intrinsic::Int64RotateRight),
    ),
    (
        "stdlib::primitives::Int64#wrappingAdd",
        I(Intrinsic::Int64AddUnchecked),
    ),
    (
        "stdlib::primitives::Int64#wrappingSub",
        I(Intrinsic::Int64SubUnchecked),
    ),
    (
        "stdlib::primitives::Int64#wrappingMul",
        I(Intrinsic::Int64MulUnchecked),
    ),
    (
        "stdlib::primitives::Int64#toUInt8",
        I(Intrinsic::Int64ToUInt8),
    ),
    (
        "stdlib::primitives::Int64#toInt32",
        I(Intrinsic::Int64ToInt32),
    ),
    (
        "stdlib::primitives::Int64#toCharUnchecked",
        I(Intrinsic::Int64ToCharUnchecked),
    ),
    (
        "stdlib::primitives::Int64#toFloat32",
        I(Intrinsic::Int64ToFloat32),
    ),
    (
        "stdlib::primitives::Int64#toFloat64",
        I(Intrinsic::Int64ToFloat64),
    ),
    (
        "stdlib::primitives::Int64#asFloat64",
        I(Intrinsic::ReinterpretInt64AsFloat64),
    ),
    // Float32
    (
        "stdlib::traits::Equals for stdlib::primitives::Float32#equals",
        I(Intrinsic::Float32Eq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::Float32#cmp",
        I(Intrinsic::Float32Cmp),
    ),
    (
        "stdlib::traits::Add for stdlib::primitives::Float32#add",
        I(Intrinsic::Float32Add),
    ),
    (
        "stdlib::traits::Sub for stdlib::primitives::Float32#sub",
        I(Intrinsic::Float32Sub),
    ),
    (
        "stdlib::traits::Mul for stdlib::primitives::Float32#mul",
        I(Intrinsic::Float32Mul),
    ),
    (
        "stdlib::traits::Div for stdlib::primitives::Float32#div",
        I(Intrinsic::Float32Div),
    ),
    (
        "stdlib::traits::Neg for stdlib::primitives::Float32#neg",
        I(Intrinsic::Float32Neg),
    ),
    ("stdlib::primitives::Float32#abs", I(Intrinsic::Float32Abs)),
    (
        "stdlib::primitives::Float32#isNan",
        I(Intrinsic::Float32IsNan),
    ),
    (
        "stdlib::primitives::Float32#roundToZero",
        I(Intrinsic::Float32RoundToZero),
    ),
    (
        "stdlib::primitives::Float32#roundUp",
        I(Intrinsic::Float32RoundUp),
    ),
    (
        "stdlib::primitives::Float32#roundDown",
        I(Intrinsic::Float32RoundDown),
    ),
    (
        "stdlib::primitives::Float32#roundHalfEven",
        I(Intrinsic::Float32RoundHalfEven),
    ),
    (
        "stdlib::primitives::Float32#sqrt",
        I(Intrinsic::Float32Sqrt),
    ),
    (
        "stdlib::primitives::Float32#toInt32",
        I(Intrinsic::Float32ToInt32),
    ),
    (
        "stdlib::primitives::Float32#toInt64",
        I(Intrinsic::Float32ToInt64),
    ),
    (
        "stdlib::primitives::Float32#toFloat64",
        I(Intrinsic::PromoteFloat32ToFloat64),
    ),
    (
        "stdlib::primitives::Float32#asInt32",
        I(Intrinsic::ReinterpretFloat32AsInt32),
    ),
    // Float64
    (
        "stdlib::traits::Equals for stdlib::primitives::Float64#equals",
        I(Intrinsic::Float64Eq),
    ),
    (
        "stdlib::traits::Comparable for stdlib::primitives::Float64#cmp",
        I(Intrinsic::Float64Cmp),
    ),
    (
        "stdlib::traits::Add for stdlib::primitives::Float64#add",
        I(Intrinsic::Float64Add),
    ),
    (
        "stdlib::traits::Sub for stdlib::primitives::Float64#sub",
        I(Intrinsic::Float64Sub),
    ),
    (
        "stdlib::traits::Mul for stdlib::primitives::Float64#mul",
        I(Intrinsic::Float64Mul),
    ),
    (
        "stdlib::traits::Div for stdlib::primitives::Float64#div",
        I(Intrinsic::Float64Div),
    ),
    (
        "stdlib::traits::Neg for stdlib::primitives::Float64#neg",
        I(Intrinsic::Float64Neg),
    ),
    ("stdlib::primitives::Float64#abs", I(Intrinsic::Float64Abs)),
    (
        "stdlib::primitives::Float64#isNan",
        I(Intrinsic::Float64IsNan),
    ),
    (
        "stdlib::primitives::Float64#roundToZero",
        I(Intrinsic::Float64RoundToZero),
    ),
    (
        "stdlib::primitives::Float64#roundUp",
        I(Intrinsic::Float64RoundUp),
    ),
    (
        "stdlib::primitives::Float64#roundDown",
        I(Intrinsic::Float64RoundDown),
    ),
    (
        "stdlib::primitives::Float64#roundHalfEven",
        I(Intrinsic::Float64RoundHalfEven),
    ),
    (
        "stdlib::primitives::Float64#sqrt",
        I(Intrinsic::Float64Sqrt),
    ),
    (
        "stdlib::primitives::Float64#toInt32",
        I(Intrinsic::Float64ToInt32),
    ),
    (
        "stdlib::primitives::Float64#toInt64",
        I(Intrinsic::Float64ToInt64),
    ),
    (
        "stdlib::primitives::Float64#toFloat32",
        I(Intrinsic::DemoteFloat64ToFloat32),
    ),
    (
        "stdlib::primitives::Float64#asInt64",
        I(Intrinsic::ReinterpretFloat64AsInt64),
    ),
    // String
    (
        "stdlib::traits::Add for stdlib::string::String#add",
        N(stdlib::strcat as *const u8),
    ),
    (
        "stdlib::string::String#clone",
        N(stdlib::str_clone as *const u8),
    ),
    (
        "stdlib::string::String#fromBytesPart",
        N(stdlib::str_from_bytes as *const u8),
    ),
    (
        "stdlib::string::String#fromStringPart",
        N(stdlib::str_from_bytes as *const u8),
    ),
    (
        "stdlib::string::String#compareTo",
        N(stdlib::strcmp as *const u8),
    ),
    (
        "stdlib::string::String#toInt32Success",
        N(stdlib::str_to_int32_success as *const u8),
    ),
    (
        "stdlib::string::String#toInt64Success",
        N(stdlib::str_to_int64_success as *const u8),
    ),
    (
        "stdlib::string::String#toInt32OrZero",
        N(stdlib::str_to_int32 as *const u8),
    ),
    (
        "stdlib::string::String#toInt64OrZero",
        N(stdlib::str_to_int64 as *const u8),
    ),
    (
        "stdlib::string::String#toFloat32Success",
        N(stdlib::str_to_float32_success as *const u8),
    ),
    (
        "stdlib::string::String#toFloat64Success",
        N(stdlib::str_to_float64_success as *const u8),
    ),
    (
        "stdlib::string::String#toFloat32OrZero",
        N(stdlib::str_to_float32 as *const u8),
    ),
    (
        "stdlib::string::String#toFloat64OrZero",
        N(stdlib::str_to_float64 as *const u8),
    ),
    ("stdlib::string::String#size", I(Intrinsic::StrLen)),
    ("stdlib::string::String#getByte", I(Intrinsic::StrGet)),
    // Array
    ("stdlib::collections::Array#size", I(Intrinsic::ArrayLen)),
    ("stdlib::collections::Array#get", I(Intrinsic::ArrayGet)),
    ("stdlib::collections::Array#set", I(Intrinsic::ArraySet)),
    (
        "stdlib::collections::Array#unsafeNew",
        I(Intrinsic::ArrayNewOfSize),
    ),
    (
        "stdlib::collections::Array#new",
        I(Intrinsic::ArrayWithValues),
    ),
    // Option
    (
        "stdlib::primitives::Option#getOrPanic",
        I(Intrinsic::OptionGetOrPanic),
    ),
    (
        "stdlib::primitives::Option#isNone",
        I(Intrinsic::OptionIsNone),
    ),
    (
        "stdlib::primitives::Option#isSome",
        I(Intrinsic::OptionIsSome),
    ),
    // AtomicInt32
    (
        "stdlib::thread::AtomicInt32#get",
        I(Intrinsic::AtomicInt32Get),
    ),
    (
        "stdlib::thread::AtomicInt32#set",
        I(Intrinsic::AtomicInt32Set),
    ),
    (
        "stdlib::thread::AtomicInt32#exchange",
        I(Intrinsic::AtomicInt32Exchange),
    ),
    (
        "stdlib::thread::AtomicInt32#compareExchange",
        I(Intrinsic::AtomicInt32CompareExchange),
    ),
    (
        "stdlib::thread::AtomicInt32#fetchAdd",
        I(Intrinsic::AtomicInt32FetchAdd),
    ),
    // AtomicInt64
    (
        "stdlib::thread::AtomicInt64#get",
        I(Intrinsic::AtomicInt64Get),
    ),
    (
        "stdlib::thread::AtomicInt64#set",
        I(Intrinsic::AtomicInt64Set),
    ),
    (
        "stdlib::thread::AtomicInt64#exchange",
        I(Intrinsic::AtomicInt64Exchange),
    ),
    (
        "stdlib::thread::AtomicInt64#compareExchange",
        I(Intrinsic::AtomicInt64CompareExchange),
    ),
    (
        "stdlib::thread::AtomicInt64#fetchAdd",
        I(Intrinsic::AtomicInt64FetchAdd),
    ),
    // Thread
    (
        "stdlib::thread::Thread#current",
        I(Intrinsic::ThreadCurrent),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::Float32#toString",
        N(stdlib::float32_to_string as *const u8),
    ),
    (
        "stdlib::string::Stringable for stdlib::primitives::Float64#toString",
        N(stdlib::float64_to_string as *const u8),
    ),
];

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

extern "C" fn abort() {
    eprintln!("program aborted.");
    std::process::exit(1);
}

extern "C" fn exit(status: i32) {
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

extern "C" fn timestamp() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let timestamp = SystemTime::now();
    timestamp.duration_since(UNIX_EPOCH).unwrap().as_millis() as u64
}

extern "C" fn println(val: Handle<Str>) {
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    handle.write(val.content()).unwrap();
    handle.write(b"\n").unwrap();
}

extern "C" fn sleep(seconds: i32) {
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

pub extern "C" fn gc_alloc(size: usize) -> *mut Object {
    let vm = get_vm();
    vm.gc.alloc(vm, size).to_mut_ptr()
}

extern "C" fn gc_collect() {
    let vm = get_vm();
    vm.gc.force_collect(vm, GcReason::ForceCollect);
}

extern "C" fn gc_minor_collect() {
    let vm = get_vm();
    vm.gc.force_collect(vm, GcReason::ForceMinorCollect);
}

extern "C" fn argc() -> i32 {
    let vm = get_vm();

    vm.program_args.len() as i32
}

extern "C" fn argv(ind: i32) -> Ref<Str> {
    let vm = get_vm();

    if ind >= 0 && (ind as usize) < vm.program_args.len() {
        let value = &vm.program_args[ind as usize];

        return Str::from_buffer(vm, value.as_bytes());
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
    };

    eprintln!("{}", msg);
    let stacktrace = stacktrace_from_last_dtn(vm);
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    stacktrace.dump(vm, &mut stderr).expect("output broken");
    unsafe {
        libc::_exit(101 + trap_id as i32);
    }
}

pub extern "C" fn stack_overflow() {
    trap(Trap::STACK_OVERFLOW as u32);
}

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
    use crate::compiler;

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

    let vtable = runner_handle.header().vtbl(vm.meta_space_start());
    let class_instance = vtable.class_instance();

    let (lambda_id, type_params) = match &class_instance.kind {
        ShapeKind::Lambda(lambda_id, type_params) => (*lambda_id, type_params.clone()),
        _ => unreachable!(),
    };

    let tld = thread.tld_address();
    let fct_ptr = compiler::compile_fct_jit(vm, lambda_id, &type_params);

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

pub extern "C" fn condition_block_after_enqueue(_cond: Handle<Object>) {
    let thread = current_thread();
    thread.block();
}

pub extern "C" fn condition_wakeup_one(cond: Handle<Object>) {
    let vm = get_vm();
    vm.wait_lists.wakeup(cond.direct_ptr());
}

pub extern "C" fn condition_wakeup_all(cond: Handle<Object>) {
    let vm = get_vm();
    vm.wait_lists.wakeup_all(cond.direct_ptr());
}
