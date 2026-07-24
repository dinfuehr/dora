use dora_bytecode::{BytecodeType, Program};

use crate::specialize_ty_in_program;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgumentPassingMode {
    None,
    Register(BytecodeType),
    Stack,
}

pub fn argument_passing_mode(program: &Program, ty: &BytecodeType) -> ArgumentPassingMode {
    let field_types = match ty {
        BytecodeType::Struct(struct_id, type_params) => {
            let struct_ = program.struct_(*struct_id);
            struct_
                .fields
                .iter()
                .map(|field| specialize_ty_in_program(program, field.ty.clone(), type_params))
                .collect::<Vec<_>>()
        }
        BytecodeType::Tuple(subtypes) => subtypes.to_vec(),
        BytecodeType::Unit => return ArgumentPassingMode::None,
        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc { .. }
        | BytecodeType::TypeParam(..)
        | BytecodeType::This => panic!("unexpected generic type {ty:?}"),
        _ => return ArgumentPassingMode::Register(ty.clone()),
    };

    let mut register_ty = None;

    for field_ty in field_types {
        match argument_passing_mode(program, &field_ty) {
            ArgumentPassingMode::None => {}
            ArgumentPassingMode::Register(field_register_ty) => {
                if register_ty.is_some() {
                    return ArgumentPassingMode::Stack;
                }
                register_ty = Some(field_register_ty);
            }
            ArgumentPassingMode::Stack => return ArgumentPassingMode::Stack,
        }
    }

    match register_ty {
        Some(register_ty) => ArgumentPassingMode::Register(register_ty),
        None => ArgumentPassingMode::None,
    }
}
