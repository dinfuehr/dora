use dora_bytecode::{BytecodeType, Program};

use crate::{register_ty, specialize_ty_in_program};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgumentPassingMode {
    None,
    Register(BytecodeType),
    Stack,
}

pub fn argument_passing_mode(program: &Program, ty: &BytecodeType) -> ArgumentPassingMode {
    let field_ty = match ty {
        BytecodeType::Struct(struct_id, type_params) => {
            let struct_ = program.struct_(*struct_id);
            if struct_.fields.len() != 1 {
                return ArgumentPassingMode::Stack;
            }

            specialize_ty_in_program(program, None, struct_.fields[0].ty.clone(), type_params)
        }
        BytecodeType::Tuple(subtypes) => {
            if subtypes.len() != 1 {
                return ArgumentPassingMode::Stack;
            }

            subtypes[0].clone()
        }
        BytecodeType::Unit => return ArgumentPassingMode::None,
        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc { .. }
        | BytecodeType::TypeParam(..)
        | BytecodeType::This => panic!("unexpected generic type {ty:?}"),
        _ => return ArgumentPassingMode::Register(register_ty(ty.clone())),
    };

    match argument_passing_mode(program, &field_ty) {
        mode @ ArgumentPassingMode::Register(_) => mode,
        ArgumentPassingMode::None | ArgumentPassingMode::Stack => ArgumentPassingMode::Stack,
    }
}
