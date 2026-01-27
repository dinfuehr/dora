use crate::{
    BytecodeType, ExtensionId, FunctionId, FunctionKind, ImplId, ModuleElementId, PackageId,
    Program, TraitId,
};

/// Resolve a path like "std::collections::Array" or "<prog>::f" to a ModuleElementId.
pub fn resolve_path(program: &Program, path: &str) -> Option<ModuleElementId> {
    let mut components = path.split("::");

    let package_name = components.next()?;
    let package_id = lookup_package(program, package_name)?;

    let package = program.package(package_id);
    let mut item = ModuleElementId::Module(package.root_module_id);

    for component in components {
        let module_id = item.module_id()?;
        let module = program.module(module_id);

        item = match module
            .items
            .binary_search_by(|(k, _)| k.as_str().cmp(component))
        {
            Ok(idx) => module.items[idx].1,
            Err(_) => return None,
        };
    }

    Some(item)
}

/// Lookup a function by path. Supports:
/// - Simple paths like "<prog>::f" or "std::io::println"
/// - Extension methods like "std::string::String#size"
/// - Trait impl methods like "std::traits::Add for std::primitives::Int32#add"
pub fn lookup_fct(program: &Program, path: &str) -> Option<FunctionId> {
    if path.contains("#") {
        let parts: Vec<_> = path.split("#").collect();
        if parts.len() != 2 {
            return None;
        }
        let container_path = parts[0];
        let method_name = parts[1];

        if container_path.contains(" for ") {
            // Trait impl method: "TraitPath for TypePath#method"
            let parts: Vec<_> = container_path.split(" for ").collect();
            if parts.len() != 2 {
                return None;
            }

            let trait_path = parts[0];
            let extended_ty_path = parts[1];

            let trait_id = resolve_path(program, trait_path)?.trait_id()?;

            let impl_id = if let Some(extended_ty) = get_primitive_ty(extended_ty_path) {
                lookup_impl_for_ty(program, trait_id, extended_ty)?
            } else {
                let extended_ty = resolve_path(program, extended_ty_path)?;
                lookup_impl_for_item(program, trait_id, extended_ty)?
            };

            lookup_fct_by_impl_id_and_name(program, impl_id, method_name)
        } else {
            // Extension method: "TypePath#method"
            let extension_id = if let Some(extended_ty) = get_primitive_ty(container_path) {
                lookup_extension_for_ty(program, extended_ty)?
            } else {
                let extended_ty = resolve_path(program, container_path)?;
                lookup_extension_for_item(program, extended_ty)?
            };

            lookup_fct_by_extension_id_and_name(program, extension_id, method_name)
        }
    } else {
        resolve_path(program, path)?.function_id()
    }
}

fn get_primitive_ty(path: &str) -> Option<BytecodeType> {
    match path {
        "std::primitives::Bool" => Some(BytecodeType::Bool),
        "std::primitives::UInt8" => Some(BytecodeType::UInt8),
        "std::primitives::Char" => Some(BytecodeType::Char),
        "std::primitives::Int32" => Some(BytecodeType::Int32),
        "std::primitives::Int64" => Some(BytecodeType::Int64),
        "std::primitives::Float32" => Some(BytecodeType::Float32),
        "std::primitives::Float64" => Some(BytecodeType::Float64),
        _ => None,
    }
}

fn lookup_package(program: &Program, name: &str) -> Option<PackageId> {
    for (id, package) in program.packages.iter().enumerate() {
        if package.name == name {
            return Some(id.into());
        }
    }
    None
}

fn lookup_fct_by_extension_id_and_name(
    program: &Program,
    extension_id: ExtensionId,
    name: &str,
) -> Option<FunctionId> {
    for (id, fct) in program.functions.iter().enumerate() {
        match fct.kind {
            FunctionKind::Extension(ext_id) if ext_id == extension_id && fct.name == name => {
                return Some(id.into());
            }
            _ => {}
        }
    }
    None
}

fn lookup_fct_by_impl_id_and_name(
    program: &Program,
    impl_id: ImplId,
    name: &str,
) -> Option<FunctionId> {
    for (id, fct) in program.functions.iter().enumerate() {
        match fct.kind {
            FunctionKind::Impl(fct_impl_id) if fct_impl_id == impl_id && fct.name == name => {
                return Some(id.into());
            }
            _ => {}
        }
    }
    None
}

fn lookup_extension_for_item(
    program: &Program,
    extended_ty: ModuleElementId,
) -> Option<ExtensionId> {
    for (id, extension) in program.extensions.iter().enumerate() {
        match &extension.extended_ty {
            BytecodeType::Class(ext_class_id, ..)
                if extended_ty == ModuleElementId::Class(*ext_class_id) =>
            {
                return Some(id.into());
            }
            BytecodeType::Enum(ext_enum_id, ..)
                if extended_ty == ModuleElementId::Enum(*ext_enum_id) =>
            {
                return Some(id.into());
            }
            _ => {}
        }
    }
    None
}

fn lookup_extension_for_ty(program: &Program, extended_ty: BytecodeType) -> Option<ExtensionId> {
    for (id, extension) in program.extensions.iter().enumerate() {
        if extension.extended_ty == extended_ty {
            return Some(id.into());
        }
    }
    None
}

fn lookup_impl_for_ty(
    program: &Program,
    trait_id: TraitId,
    extended_ty: BytecodeType,
) -> Option<ImplId> {
    for (id, impl_) in program.impls.iter().enumerate() {
        if impl_.trait_ty.trait_id != trait_id {
            continue;
        }
        if impl_.extended_ty == extended_ty {
            return Some(id.into());
        }
    }
    None
}

fn lookup_impl_for_item(
    program: &Program,
    trait_id: TraitId,
    extended_ty: ModuleElementId,
) -> Option<ImplId> {
    for (id, impl_) in program.impls.iter().enumerate() {
        if impl_.trait_ty.trait_id != trait_id {
            continue;
        }
        match impl_.extended_ty {
            BytecodeType::Class(class_id, ..)
                if extended_ty == ModuleElementId::Class(class_id) =>
            {
                return Some(id.into());
            }
            _ => {}
        }
    }
    None
}
