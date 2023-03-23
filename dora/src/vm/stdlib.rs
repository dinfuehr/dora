use std::collections::HashMap;

use crate::gc::Address;
use crate::stack;
use crate::stdlib;
use crate::vm::VM;
use dora_bytecode::program::InternalClass;
use dora_bytecode::program::InternalFunction;
use dora_bytecode::{ClassId, FunctionId, NativeFunction};

pub fn resolve_native_functions(vm: &mut VM) {
    let mut mappings: HashMap<NativeFunction, *const u8> = HashMap::from([
        (NativeFunction::Abort, stdlib::abort as *const u8),
        (NativeFunction::Exit, stdlib::exit as *const u8),
        (NativeFunction::FatalError, stdlib::fatal_error as *const u8),
        (NativeFunction::Print, stdlib::print as *const u8),
        (NativeFunction::PrintLn, stdlib::println as *const u8),
        (NativeFunction::Argc, stdlib::argc as *const u8),
        (NativeFunction::Argv, stdlib::argv as *const u8),
        (
            NativeFunction::ForceCollect,
            stdlib::gc_collect as *const u8,
        ),
        (
            NativeFunction::ForceMinorCollect,
            stdlib::gc_minor_collect as *const u8,
        ),
        (NativeFunction::Timestamp, stdlib::timestamp as *const u8),
        (NativeFunction::Sleep, stdlib::sleep as *const u8),
        (
            NativeFunction::UInt8ToString,
            stdlib::uint8_to_string as *const u8,
        ),
        (
            NativeFunction::CharToString,
            stdlib::char_to_string as *const u8,
        ),
        (
            NativeFunction::Int32ToString,
            stdlib::int32_to_string as *const u8,
        ),
        (
            NativeFunction::Int64ToString,
            stdlib::int64_to_string as *const u8,
        ),
        (NativeFunction::StringCompareTo, stdlib::strcmp as *const u8),
        (
            NativeFunction::StringToInt32Success,
            stdlib::str_to_int32_success as *const u8,
        ),
        (
            NativeFunction::StringToInt64Success,
            stdlib::str_to_int64_success as *const u8,
        ),
        (
            NativeFunction::StringToFloat32Success,
            stdlib::str_to_float32_success as *const u8,
        ),
        (
            NativeFunction::StringToFloat64Success,
            stdlib::str_to_float64_success as *const u8,
        ),
        (
            NativeFunction::StringToInt32OrZero,
            stdlib::str_to_int32 as *const u8,
        ),
        (
            NativeFunction::StringToInt64OrZero,
            stdlib::str_to_int64 as *const u8,
        ),
        (
            NativeFunction::StringToFloat32OrZero,
            stdlib::str_to_float32 as *const u8,
        ),
        (
            NativeFunction::StringToFloat64OrZero,
            stdlib::str_to_float64 as *const u8,
        ),
        (NativeFunction::StringPlus, stdlib::strcat as *const u8),
        (
            NativeFunction::Float32ToString,
            stdlib::float32_to_string as *const u8,
        ),
        (
            NativeFunction::Float64ToString,
            stdlib::float64_to_string as *const u8,
        ),
        (
            NativeFunction::StringFromBytesPart,
            stdlib::str_from_bytes as *const u8,
        ),
        (
            NativeFunction::StringFromStringPart,
            stdlib::str_from_bytes as *const u8,
        ),
        (
            NativeFunction::RetrieveStacktrace,
            stack::retrieve_stack_trace as *const u8,
        ),
        (
            NativeFunction::GetStackTraceElement,
            stack::stack_element as *const u8,
        ),
        (
            NativeFunction::SpawnThread,
            stdlib::spawn_thread as *const u8,
        ),
        (NativeFunction::ThreadJoin, stdlib::join_thread as *const u8),
        (NativeFunction::MutexWait, stdlib::mutex_wait as *const u8),
        (
            NativeFunction::MutexNotify,
            stdlib::mutex_notify as *const u8,
        ),
        (
            NativeFunction::ConditionEnqueue,
            stdlib::condition_enqueue as *const u8,
        ),
        (
            NativeFunction::ConditionBlock,
            stdlib::condition_block_after_enqueue as *const u8,
        ),
        (
            NativeFunction::ConditionWakupOne,
            stdlib::condition_wakeup_one as *const u8,
        ),
        (
            NativeFunction::ConditionWakupAll,
            stdlib::condition_wakeup_all as *const u8,
        ),
        (
            NativeFunction::ReadFileAsString,
            stdlib::io::read_file_as_string as *const u8,
        ),
        (
            NativeFunction::ReadFileAsBytes,
            stdlib::io::read_file_as_bytes as *const u8,
        ),
        (
            NativeFunction::WriteFileAsString,
            stdlib::io::write_file_as_string as *const u8,
        ),
        (
            NativeFunction::WriteFileAsBytes,
            stdlib::io::write_file_as_bytes as *const u8,
        ),
        (
            NativeFunction::SocketConnect,
            stdlib::io::socket_connect as *const u8,
        ),
        (
            NativeFunction::SocketClose,
            stdlib::io::socket_close as *const u8,
        ),
        (
            NativeFunction::SocketWrite,
            stdlib::io::socket_write as *const u8,
        ),
        (
            NativeFunction::SocketRead,
            stdlib::io::socket_read as *const u8,
        ),
        (
            NativeFunction::SocketBind,
            stdlib::io::socket_bind as *const u8,
        ),
        (
            NativeFunction::SocketAccept,
            stdlib::io::socket_accept as *const u8,
        ),
        (NativeFunction::StringClone, stdlib::str_clone as *const u8),
    ]);

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if let Some(native_function) = fct.native {
            if let Some(ptr) = mappings.remove(&native_function) {
                vm.native_implementations
                    .insert(fct_id, Address::from_ptr(ptr));
            }
        }
    }

    assert!(mappings.is_empty());
}

pub fn resolve_internal_classes(vm: &mut VM) {
    for (cls_id, cls) in vm.program.classes.iter().enumerate() {
        let cls_id = ClassId(cls_id as u32);

        if let Some(internal_class) = cls.internal {
            match internal_class {
                InternalClass::Array => vm.known_instances.array_class_id = Some(cls_id),
                InternalClass::String => vm.known_instances.string_class_id = Some(cls_id),
                InternalClass::Thread => vm.known_instances.thread_class_id = Some(cls_id),
                InternalClass::StacktraceElement => {
                    vm.known_instances.stacktrace_element_class_id = Some(cls_id)
                }
            }
        }
    }
}

pub fn resolve_internal_functions(vm: &mut VM) {
    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if let Some(internal_function) = fct.internal {
            match internal_function {
                InternalFunction::BootsCompile => {
                    vm.known_instances.boots_compile_fct_id = Some(fct_id);
                }
                InternalFunction::StacktraceRetrieve => {
                    vm.known_instances.stacktrace_retrieve_fct_id = Some(fct_id);
                }
            }
        }
    }
}
