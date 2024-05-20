use std::collections::HashMap;

use crate::boots::BOOTS_NATIVE_FUNCTIONS;
use crate::gc::Address;
use crate::stack;
use crate::stdlib::io::IO_NATIVE_FUNCTIONS;
use crate::stdlib::{self, STDLIB_NATIVE_FUNCTIONS};
use crate::vm::VM;
use dora_bytecode::ModuleId;
use dora_bytecode::{ClassId, FunctionId, NativeFunction, PackageId};

pub fn connect_native_functions_to_implementation(vm: &mut VM) {
    for (path, ptr) in STDLIB_NATIVE_FUNCTIONS {
        native_fct(vm, path, *ptr);
    }

    for (path, ptr) in IO_NATIVE_FUNCTIONS {
        native_fct(vm, path, *ptr);
    }

    let mut mappings: HashMap<NativeFunction, *const u8> = HashMap::from([
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
        (NativeFunction::StringClone, stdlib::str_clone as *const u8),
    ]);

    if vm.program.boots_package_id.is_some() {
        for (path, ptr) in BOOTS_NATIVE_FUNCTIONS {
            native_fct(vm, path, *ptr);
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

fn native_fct(vm: &mut VM, full_path: &str, ptr: *const u8) {
    let fct_id = find_fct(vm, full_path);

    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));

    if old.is_some() {
        panic!("function {} was already initialized", full_path);
    }

    assert!(old.is_none());
}

pub fn find_fct(vm: &VM, full_path: &str) -> FunctionId {
    let mut components = full_path.split("::");

    let package_name = components.next().expect("missing package name");
    let package_id = match lookup_package(vm, package_name) {
        Some(package_id) => package_id,
        None => {
            panic!("unknown package {} in path {}", package_name, full_path);
        }
    };

    let mut path = Vec::new();

    while let Some(component) = components.next() {
        path.push(component);
    }

    let fct_name = path.pop().expect("missing function name");

    let package = &vm.program.packages[package_id.0 as usize];
    let mut module_id = package.root_module_id;

    for component in path {
        module_id = match lookup_module(vm, module_id, component) {
            Some(next_module_id) => next_module_id,
            None => {
                panic!("unknown module {} in path {}", component, full_path);
            }
        };
    }

    match lookup_fct(vm, module_id, fct_name) {
        Some(fct_id) => fct_id,
        None => {
            panic!("unknown function {} in path {}", fct_name, full_path);
        }
    }
}

fn find_class(vm: &VM, full_path: &str) -> ClassId {
    let mut components = full_path.split("::");

    let package_name = components.next().expect("missing package name");
    let package_id = match lookup_package(vm, package_name) {
        Some(package_id) => package_id,
        None => {
            panic!("unknown package {} in path {}", package_name, full_path);
        }
    };

    let mut path = Vec::new();

    while let Some(component) = components.next() {
        path.push(component);
    }

    let fct_name = path.pop().expect("missing function name");

    let package = &vm.program.packages[package_id.0 as usize];
    let mut module_id = package.root_module_id;

    for component in path {
        module_id = match lookup_module(vm, module_id, component) {
            Some(next_module_id) => next_module_id,
            None => {
                panic!("unknown module {} in path {}", component, full_path);
            }
        };
    }

    match lookup_class(vm, module_id, fct_name) {
        Some(fct_id) => fct_id,
        None => {
            panic!("unknown function {} in path {}", fct_name, full_path);
        }
    }
}

fn lookup_package(vm: &VM, name: &str) -> Option<PackageId> {
    for (id, package) in vm.program.packages.iter().enumerate() {
        if package.name == name {
            return Some(PackageId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_module(vm: &VM, module_id: ModuleId, name: &str) -> Option<ModuleId> {
    for (id, module) in vm.program.modules.iter().enumerate() {
        if module.parent_id == Some(module_id) && module.name == name {
            return Some(ModuleId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_fct(vm: &VM, module_id: ModuleId, name: &str) -> Option<FunctionId> {
    for (id, fct) in vm.program.functions.iter().enumerate() {
        if fct.module_id == module_id && fct.name == name {
            return Some(FunctionId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_class(vm: &VM, module_id: ModuleId, name: &str) -> Option<ClassId> {
    for (id, class) in vm.program.classes.iter().enumerate() {
        if class.module_id == module_id && class.name == name {
            return Some(ClassId(id.try_into().expect("overflow")));
        }
    }

    None
}

pub fn lookup_known_classes(vm: &mut VM) {
    vm.known.array_class_id = Some(find_class(vm, "stdlib::collections::Array"));
    vm.known.string_class_id = Some(find_class(vm, "stdlib::string::String"));
    vm.known.thread_class_id = Some(find_class(vm, "stdlib::thread::Thread"));
    vm.known.stacktrace_element_class_id = Some(find_class(vm, "stdlib::StacktraceElement"));
}

pub fn lookup_known_functions(vm: &mut VM) {
    if vm.program.boots_package_id.is_some() {
        vm.known.boots_compile_fct_id = Some(find_fct(vm, "boots::interface::compile"));
    }

    vm.known.stacktrace_retrieve_fct_id = Some(find_fct(vm, "stdlib::retrieveStacktrace"));
}
