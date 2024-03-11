use std::collections::HashMap;

use crate::boots;
use crate::gc::Address;
use crate::stack;
use crate::stdlib;
use crate::vm::VM;
use dora_bytecode::program::InternalClass;
use dora_bytecode::program::InternalFunction;
use dora_bytecode::ModuleId;
use dora_bytecode::{ClassId, FunctionId, NativeFunction, PackageId};

pub fn connect_native_functions_to_implementation(vm: &mut VM) {
    native_fct(
        vm,
        vm.program.stdlib_package_id,
        "abort",
        stdlib::abort as *const u8,
    );

    native_fct(
        vm,
        vm.program.stdlib_package_id,
        "exit",
        stdlib::exit as *const u8,
    );

    native_fct(
        vm,
        vm.program.stdlib_package_id,
        "fatalError",
        stdlib::fatal_error as *const u8,
    );

    native_fct(
        vm,
        vm.program.stdlib_package_id,
        "print",
        stdlib::print as *const u8,
    );

    native_fct(
        vm,
        vm.program.stdlib_package_id,
        "println",
        stdlib::println as *const u8,
    );

    let mut mappings: HashMap<NativeFunction, *const u8> = HashMap::from([
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

    let boots_mappings: HashMap<NativeFunction, *const u8> = HashMap::from([
        (
            NativeFunction::BootsGetSystemConfig,
            boots::get_system_config as *const u8,
        ),
        (
            NativeFunction::BootsGetFunctionAddress,
            boots::get_function_address as *const u8,
        ),
        (
            NativeFunction::BootsGetClassPointerForLambda,
            boots::get_class_pointer_for_lambda as *const u8,
        ),
        (
            NativeFunction::BootsGetGlobalValueAddressRaw,
            boots::get_global_value_address as *const u8,
        ),
        (
            NativeFunction::BootsGetGlobalStateAddressRaw,
            boots::get_global_state_address as *const u8,
        ),
        (
            NativeFunction::BootsHasGlobalInitialValueRaw,
            boots::has_global_initial_value as *const u8,
        ),
        (
            NativeFunction::BootsGetFunctionVtableIndex,
            boots::get_function_vtable_index as *const u8,
        ),
        (
            NativeFunction::BootsGetFieldOffset,
            boots::get_field_offset as *const u8,
        ),
        (
            NativeFunction::BootsGetClassSize,
            boots::get_class_size as *const u8,
        ),
        (
            NativeFunction::BootsGetClassPointer,
            boots::get_class_pointer as *const u8,
        ),
    ]);

    if vm.program.boots_package_id.is_some() {
        for (nf, address) in boots_mappings {
            assert!(mappings.insert(nf, address).is_none());
        }
    }

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if let Some(native_function) = fct.native {
            if let Some(ptr) = mappings.remove(&native_function) {
                let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));
                assert!(old.is_none());
            } else if vm.native_methods.get(fct_id).is_none() {
                panic!("unknown native function {}", fct.name);
            }
        }
    }

    assert!(mappings.is_empty());
}

fn native_fct(vm: &mut VM, package_id: PackageId, name: &str, ptr: *const u8) {
    let package = &vm.program.packages[package_id.0 as usize];
    let fct_id = lookup_fct(vm, package.root_module_id, name);
    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));
    assert!(old.is_none());
}

fn lookup_fct(vm: &mut VM, module_id: ModuleId, name: &str) -> FunctionId {
    for (id, fct) in vm.program.functions.iter().enumerate() {
        if fct.module_id == module_id && fct.name == name {
            return FunctionId(id.try_into().expect("overflow"));
        }
    }

    panic!("function {} not found.", name)
}

pub fn lookup_known_classes(vm: &mut VM) {
    for (cls_id, cls) in vm.program.classes.iter().enumerate() {
        let cls_id = ClassId(cls_id as u32);

        if let Some(internal_class) = cls.internal {
            match internal_class {
                InternalClass::Array => vm.known.array_class_id = Some(cls_id),
                InternalClass::String => vm.known.string_class_id = Some(cls_id),
                InternalClass::Thread => vm.known.thread_class_id = Some(cls_id),
                InternalClass::StacktraceElement => {
                    vm.known.stacktrace_element_class_id = Some(cls_id)
                }
            }
        }
    }
}

pub fn lookup_known_functions(vm: &mut VM) {
    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if let Some(internal_function) = fct.internal {
            match internal_function {
                InternalFunction::BootsCompile => {
                    vm.known.boots_compile_fct_id = Some(fct_id);
                }
                InternalFunction::StacktraceRetrieve => {
                    vm.known.stacktrace_retrieve_fct_id = Some(fct_id);
                }
            }
        }
    }
}
