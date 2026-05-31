use dora_bytecode::{BytecodeType, BytecodeTypeArray, TraitId};

pub trait BytecodeTypeExt {
    fn type_param_id(&self) -> Option<u32>;
    fn trait_id(&self) -> Option<TraitId>;
    fn is_zeroable_primitive(&self) -> bool;
    fn type_params(&self) -> BytecodeTypeArray;
}

impl BytecodeTypeExt for BytecodeType {
    fn type_param_id(&self) -> Option<u32> {
        match self {
            BytecodeType::TypeParam(tp_id) => Some(*tp_id),
            _ => None,
        }
    }

    fn trait_id(&self) -> Option<TraitId> {
        match self {
            BytecodeType::TraitObject(trait_id, ..) => Some(*trait_id),
            _ => None,
        }
    }

    fn is_zeroable_primitive(&self) -> bool {
        match self {
            &BytecodeType::Bool
            | &BytecodeType::UInt8
            | &BytecodeType::Char
            | &BytecodeType::Int32
            | &BytecodeType::Int64
            | &BytecodeType::Float32
            | &BytecodeType::Float64 => true,
            BytecodeType::TypeAlias(..) | BytecodeType::Assoc { .. } => unreachable!(),
            &BytecodeType::Unit
            | &BytecodeType::Tuple(..)
            | &BytecodeType::Enum(..)
            | &BytecodeType::Struct(..)
            | &BytecodeType::Class(..)
            | &BytecodeType::TraitObject(..)
            | &BytecodeType::Lambda(..)
            | &BytecodeType::TypeParam(..)
            | &BytecodeType::Ptr
            | &BytecodeType::Address
            | &BytecodeType::This
            | &BytecodeType::Ref(..) => false,
        }
    }

    fn type_params(&self) -> BytecodeTypeArray {
        match self {
            BytecodeType::Class(_, params)
            | BytecodeType::Enum(_, params)
            | BytecodeType::Struct(_, params)
            | BytecodeType::TraitObject(_, params, ..) => params.clone(),
            BytecodeType::TypeAlias(..) | BytecodeType::Assoc { .. } => unreachable!(),
            &BytecodeType::Bool
            | &BytecodeType::UInt8
            | &BytecodeType::Char
            | &BytecodeType::Int32
            | &BytecodeType::Int64
            | &BytecodeType::Float32
            | &BytecodeType::Float64
            | &BytecodeType::Unit
            | &BytecodeType::Tuple(..)
            | &BytecodeType::Lambda(..)
            | &BytecodeType::TypeParam(..)
            | &BytecodeType::Ptr
            | &BytecodeType::Address
            | &BytecodeType::This
            | &BytecodeType::Ref(..) => BytecodeTypeArray::empty(),
        }
    }
}
