use crate::mangle_name;
use dora_bytecode::{
    BytecodeTraitType, BytecodeType, FunctionId, FunctionKind, Program, display_fct,
    module_path_name,
};

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
