use crate::mangle_name;
use crate::stdlib::STDLIB_INTRINSICS;
use crate::vm::{Intrinsic, VM};
use dora_bytecode::{
    BytecodeTraitType, BytecodeType, FunctionId, FunctionKind, ModuleElementId, Program,
    display_fct, module_path_name,
};

pub fn lookup(vm: &mut VM) {
    for (path, intrinsic) in STDLIB_INTRINSICS {
        apply_intrinsic(vm, path, *intrinsic);
    }

    lookup_known_classes(vm);
    lookup_known_functions(vm);
    lookup_known_traits(vm);

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id: FunctionId = fct_id.into();

        if fct.is_native && fct.bytecode.is_some() {
            panic!(
                "native function {} has bytecode",
                display_fct(&vm.program, fct_id)
            );
        }

        if fct.is_internal && !fct.is_native && vm.intrinsics.get(&fct_id).is_none() {
            panic!(
                "unknown internal function {}",
                display_fct(&vm.program, fct_id)
            );
        }
    }
}

fn apply_intrinsic(vm: &mut VM, path: &str, intrinsic: Intrinsic) {
    let fct_id = lookup_fct(&vm.program, path);

    let existed = vm.intrinsics.insert(fct_id, intrinsic).is_some();

    if existed {
        panic!(
            "function {} was already initialized",
            display_fct(&vm.program, fct_id)
        );
    }
}

pub(crate) fn native_function_symbol(program: &Program, fct_id: FunctionId) -> String {
    mangle_name(&native_function_path(program, fct_id))
}

pub(crate) fn native_function_path(program: &Program, fct_id: FunctionId) -> String {
    let fct = program.fct(fct_id);

    match fct.kind {
        FunctionKind::Function => module_path_name(program, fct.module_id, &fct.name),
        FunctionKind::Extension(extension_id) => {
            let extension = program.extension(extension_id);
            format!(
                "{}#{}",
                bare_type_path(program, &extension.extended_ty),
                fct.name
            )
        }
        FunctionKind::Impl(impl_id) => {
            let impl_ = program.impl_(impl_id);
            format!(
                "{} for {}#{}",
                trait_path(program, &impl_.trait_ty),
                bare_type_path(program, &impl_.extended_ty),
                fct.name
            )
        }
        FunctionKind::Trait(_) | FunctionKind::Lambda => {
            panic!("{} cannot be native", display_fct(program, fct_id));
        }
    }
}

fn trait_path(program: &Program, trait_ty: &BytecodeTraitType) -> String {
    let trait_ = program.trait_(trait_ty.trait_id);
    module_path_name(program, trait_.module_id, &trait_.name)
}

fn bare_type_path(program: &Program, ty: &BytecodeType) -> String {
    match ty {
        BytecodeType::Bool => "std::primitives::Bool".to_string(),
        BytecodeType::UInt8 => "std::primitives::UInt8".to_string(),
        BytecodeType::Char => "std::primitives::Char".to_string(),
        BytecodeType::Int32 => "std::primitives::Int32".to_string(),
        BytecodeType::Int64 => "std::primitives::Int64".to_string(),
        BytecodeType::Float32 => "std::primitives::Float32".to_string(),
        BytecodeType::Float64 => "std::primitives::Float64".to_string(),
        BytecodeType::Class(id, _) => {
            let cls = program.class(*id);
            module_path_name(program, cls.module_id, &cls.name)
        }
        BytecodeType::Struct(id, _) => {
            let struct_ = program.struct_(*id);
            module_path_name(program, struct_.module_id, &struct_.name)
        }
        BytecodeType::Enum(id, _) => {
            let enum_ = program.enum_(*id);
            module_path_name(program, enum_.module_id, &enum_.name)
        }
        BytecodeType::Ref(inner) => bare_type_path(program, inner),
        _ => panic!("unsupported native receiver type {:?}", ty),
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
        resolve_path(&vm.program, "std::fatal_error")
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
