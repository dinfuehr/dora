use crate::gc::Address;
use crate::stack;
use crate::stdlib;
use crate::vm::{module_path, VM};
use dora_frontend::language::sem_analysis::{
    ClassDefinitionId, ExtensionDefinitionId, FctDefinitionId, Intrinsic, ModuleDefinitionId,
};
use dora_frontend::language::sym::Sym;

pub fn resolve_internal_functions(vm: &mut VM) {
    let stdlib_id = vm.stdlib_module_id();
    native_fct(
        vm,
        stdlib_id,
        "fatalError",
        stdlib::fatal_error as *const u8,
    );
    native_fct(vm, stdlib_id, "abort", stdlib::abort as *const u8);
    native_fct(vm, stdlib_id, "exit", stdlib::exit as *const u8);
    intrinsic_fct(vm, stdlib_id, "unreachable", Intrinsic::Unreachable);

    native_fct(vm, stdlib_id, "print", stdlib::print as *const u8);
    native_fct(vm, stdlib_id, "println", stdlib::println as *const u8);
    let fid = intrinsic_fct(vm, stdlib_id, "assert", Intrinsic::Assert);
    vm.known.functions.assert = Some(fid);
    intrinsic_fct(vm, stdlib_id, "debug", Intrinsic::Debug);
    native_fct(vm, stdlib_id, "argc", stdlib::argc as *const u8);
    native_fct(vm, stdlib_id, "argv", stdlib::argv as *const u8);
    native_fct(
        vm,
        stdlib_id,
        "forceCollect",
        stdlib::gc_collect as *const u8,
    );
    native_fct(vm, stdlib_id, "timestamp", stdlib::timestamp as *const u8);
    native_fct(
        vm,
        stdlib_id,
        "forceMinorCollect",
        stdlib::gc_minor_collect as *const u8,
    );
    native_fct(vm, stdlib_id, "sleep", stdlib::sleep as *const u8);

    intrinsic_fct(vm, stdlib_id, "unsafeKillRefs", Intrinsic::UnsafeKillRefs);

    native_method(
        vm,
        stdlib_id,
        "primitives::UInt8",
        "toString",
        stdlib::uint8_to_string as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "primitives::Char",
        "toString",
        stdlib::char_to_string as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "primitives::Int32",
        "toString",
        stdlib::int32_to_string as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "primitives::Int64",
        "toString",
        stdlib::int64_to_string as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "string::String",
        "compareTo",
        stdlib::strcmp as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toInt32Success",
        stdlib::str_to_int32_success as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toInt64Success",
        stdlib::str_to_int64_success as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toFloat32Success",
        stdlib::str_to_float32_success as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toFloat64Success",
        stdlib::str_to_float64_success as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toInt32OrZero",
        stdlib::str_to_int32 as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toInt64OrZero",
        stdlib::str_to_int64 as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toFloat32OrZero",
        stdlib::str_to_float32 as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "toFloat64OrZero",
        stdlib::str_to_float64 as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "string::String",
        "plus",
        stdlib::strcat as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "primitives::Float32",
        "toString",
        stdlib::float32_to_string as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "primitives::Float64",
        "toString",
        stdlib::float64_to_string as *const u8,
    );

    native_static(
        vm,
        stdlib_id,
        "string::String",
        "fromBytesPart",
        stdlib::str_from_bytes as *const u8,
    );
    native_static(
        vm,
        stdlib_id,
        "string::String",
        "fromStringPart",
        stdlib::str_from_bytes as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "Stacktrace",
        "retrieveStacktrace",
        stack::retrieve_stack_trace as *const u8,
    );
    native_method(
        vm,
        stdlib_id,
        "Stacktrace",
        "getStacktraceElement",
        stack::stack_element as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "thread::spawn",
        stdlib::spawn_thread as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Thread",
        "join",
        stdlib::join_thread as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Mutex",
        "wait",
        stdlib::mutex_wait as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Mutex",
        "notify",
        stdlib::mutex_notify as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Condition",
        "enqueue",
        stdlib::condition_enqueue as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Condition",
        "block",
        stdlib::condition_block_after_enqueue as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Condition",
        "wakeupOne",
        stdlib::condition_wakeup_one as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "thread::Condition",
        "wakeupAll",
        stdlib::condition_wakeup_all as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::readFileAsString",
        stdlib::io::read_file_as_string as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::readFileAsBytes",
        stdlib::io::read_file_as_bytes as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::writeFileAsString",
        stdlib::io::write_file_as_string as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::writeFileAsBytes",
        stdlib::io::write_file_as_bytes as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketConnect",
        stdlib::io::socket_connect as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketClose",
        stdlib::io::socket_close as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketWrite",
        stdlib::io::socket_write as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketRead",
        stdlib::io::socket_read as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketBind",
        stdlib::io::socket_bind as *const u8,
    );

    native_fct(
        vm,
        stdlib_id,
        "io::socketAccept",
        stdlib::io::socket_accept as *const u8,
    );

    native_method(
        vm,
        stdlib_id,
        "string::String",
        "clone",
        stdlib::str_clone as *const u8,
    );
}

enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(Address),
}

fn native_fct(vm: &mut VM, module_id: ModuleDefinitionId, name: &str, fctptr: *const u8) {
    common_fct(
        vm,
        module_id,
        name,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn intrinsic_fct(
    vm: &mut VM,
    module_id: ModuleDefinitionId,
    name: &str,
    intrinsic: Intrinsic,
) -> FctDefinitionId {
    common_fct(vm, module_id, name, FctImplementation::Intrinsic(intrinsic))
}

fn native_method(
    vm: &mut VM,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        vm,
        module_id,
        container_name,
        method_name,
        false,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn native_static(
    vm: &mut VM,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    fctptr: *const u8,
) {
    common_method(
        vm,
        module_id,
        container_name,
        method_name,
        true,
        FctImplementation::Native(Address::from_ptr(fctptr)),
    );
}

fn common_fct(
    vm: &mut VM,
    module_id: ModuleDefinitionId,
    name: &str,
    kind: FctImplementation,
) -> FctDefinitionId {
    let fct_id = resolve_name(vm, name, module_id)
        .to_fct()
        .expect("function expected");

    let fct = vm.fcts.idx(fct_id);
    let mut fct = fct.write();

    match kind {
        FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
        FctImplementation::Native(address) => {
            vm.native_implementations.insert(fct_id, address);
        }
    }
    fct.internal_resolved = true;
    fct_id
}

fn resolve_name(vm: &VM, name: &str, module_id: ModuleDefinitionId) -> Sym {
    let path = name.split("::");
    let mut sym = Sym::Module(module_id);

    for name in path {
        let module_id = sym.to_module().expect("module expected");
        let table = vm.modules.idx(module_id).read().table.clone();
        let table = table.read();

        let interned_name = vm.interner.intern(name);

        if let Some(current_sym) = table.get(interned_name) {
            sym = current_sym;
        } else {
            panic!(
                "{} not found in module {}.",
                name,
                module_path(vm, module_id)
            );
        }
    }

    sym
}

fn common_method(
    vm: &mut VM,
    module_id: ModuleDefinitionId,
    container_name: &str,
    method_name: &str,
    is_static: bool,
    implementation: FctImplementation,
) -> FctDefinitionId {
    let sym = resolve_name(vm, container_name, module_id);

    match sym {
        Sym::Class(cls_id) => {
            internal_class_method(vm, cls_id, method_name, is_static, implementation)
        }

        Sym::Struct(struct_id) => {
            let struct_ = vm.structs.idx(struct_id);
            let struct_ = struct_.read();
            internal_extension_method(
                vm,
                &struct_.extensions,
                method_name,
                is_static,
                implementation,
            )
        }
        Sym::Enum(enum_id) => {
            let enum_ = vm.enums.idx(enum_id);
            let enum_ = enum_.read();
            internal_extension_method(
                vm,
                &enum_.extensions,
                method_name,
                is_static,
                implementation,
            )
        }

        _ => panic!("unexpected type"),
    }
}

fn internal_class_method(
    vm: &mut VM,
    cls_id: ClassDefinitionId,
    name: &str,
    is_static: bool,
    kind: FctImplementation,
) -> FctDefinitionId {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    internal_extension_method(vm, &cls.extensions, name, is_static, kind)
}

fn internal_extension_method(
    vm: &mut VM,
    extensions: &[ExtensionDefinitionId],
    name_as_string: &str,
    is_static: bool,
    kind: FctImplementation,
) -> FctDefinitionId {
    let name = vm.interner.intern(name_as_string);

    for &extension_id in extensions {
        let extension = vm.extensions[extension_id].read();

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.get(&name) {
            let fct = vm.fcts.idx(method_id);
            let mut fct = fct.write();

            match kind {
                FctImplementation::Intrinsic(intrinsic) => fct.intrinsic = Some(intrinsic),
                FctImplementation::Native(address) => {
                    vm.native_implementations.insert(method_id, address);
                }
            }
            fct.internal_resolved = true;
            return method_id;
        }
    }

    panic!("method {} not found!", name_as_string)
}
