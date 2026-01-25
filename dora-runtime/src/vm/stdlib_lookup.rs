use crate::boots::BOOTS_FUNCTIONS;
use crate::gc::Address;
use crate::stdlib::STDLIB_FUNCTIONS;
use crate::stdlib::io::IO_FUNCTIONS;
use crate::vm::{Intrinsic, VM};
use dora_bytecode::{FunctionId, ModuleElementId, Program, display_fct};

#[derive(Clone)]
pub enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(*const u8),
}

pub fn lookup(vm: &mut VM) {
    for (path, implementation) in STDLIB_FUNCTIONS {
        apply_fct(vm, path, implementation.clone());
    }

    for (path, implementation) in IO_FUNCTIONS {
        apply_fct(vm, path, implementation.clone());
    }

    if vm.has_boots() {
        for (path, implementation) in BOOTS_FUNCTIONS {
            apply_fct(vm, path, implementation.clone());
        }
    }

    lookup_known_classes(vm);
    lookup_known_functions(vm);
    lookup_known_traits(vm);

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if fct.is_internal
            && vm.native_methods.get(fct_id).is_none()
            && vm.intrinsics.get(&fct_id).is_none()
        {
            panic!(
                "unknown internal function {}",
                display_fct(&vm.program, fct_id)
            );
        }
    }
}

fn apply_fct(vm: &mut VM, path: &str, implementation: FctImplementation) {
    let fct_id = lookup_fct(&vm.program, path);

    let existed = match implementation {
        FctImplementation::Intrinsic(intrinsic) => {
            vm.intrinsics.insert(fct_id, intrinsic).is_some()
        }
        FctImplementation::Native(ptr) => vm
            .native_methods
            .insert(fct_id, Address::from_ptr(ptr))
            .is_some(),
    };

    if existed {
        panic!(
            "function {} was already initialized",
            display_fct(&vm.program, fct_id)
        );
    }
}

fn lookup_known_classes(vm: &mut VM) {
    vm.known.array_class_id = Some(
        resolve_path(&vm.program, "std::collections::Array")
            .class_id()
            .expect("class expected"),
    );
    vm.known.string_class_id = Some(
        resolve_path(&vm.program, "std::string::String")
            .class_id()
            .expect("class expected"),
    );
    vm.known.thread_class_id = Some(
        resolve_path(&vm.program, "std::thread::Thread")
            .class_id()
            .expect("class expected"),
    );
}

fn lookup_known_functions(vm: &mut VM) {
    if vm.has_boots() {
        vm.known.boots_compile_fct_id = Some(
            resolve_path(&vm.program, "boots::interface::compile")
                .function_id()
                .expect("function expected"),
        );
    }

    vm.known.unreachable_fct_id = Some(
        resolve_path(&vm.program, "std::unreachable")
            .function_id()
            .expect("function expected"),
    );

    vm.known.fatal_error_fct_id = Some(
        resolve_path(&vm.program, "std::fatalError")
            .function_id()
            .expect("function expected"),
    );
}

fn lookup_known_traits(vm: &mut VM) {
    vm.known.zero_trait_id = Some(
        resolve_path(&vm.program, "std::traits::Zero")
            .trait_id()
            .expect("trait expected"),
    );
}

fn resolve_path(program: &Program, path: &str) -> ModuleElementId {
    use dora_bytecode::resolve_path;
    resolve_path(program, path).unwrap_or_else(|| panic!("'{}' not found", path))
}

fn lookup_fct(program: &Program, path: &str) -> FunctionId {
    use dora_bytecode::lookup_fct;
    lookup_fct(program, path).unwrap_or_else(|| panic!("'{}' not found", path))
}
