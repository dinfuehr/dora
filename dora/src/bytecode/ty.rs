use std::sync::Arc;

use crate::bytecode::BytecodeTypeKind;
use crate::language::sem_analysis::{
    ClassDefinitionId, EnumDefinitionId, TraitDefinitionId, ValueDefinitionId,
};
use crate::language::ty::{SourceType, SourceTypeArray};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BytecodeType {
    Unit,
    Bool,
    UInt8,
    Char,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
    Tuple(SourceTypeArray),
    TypeParam(u32),
    Enum(EnumDefinitionId, SourceTypeArray),
    Value(ValueDefinitionId, SourceTypeArray),
    Class(ClassDefinitionId, SourceTypeArray),
    Trait(TraitDefinitionId, SourceTypeArray),
    Lambda(SourceTypeArray, Box<SourceType>),
}

impl BytecodeType {
    pub fn kind(&self) -> BytecodeTypeKind {
        match self {
            BytecodeType::Unit => BytecodeTypeKind::Unit,
            BytecodeType::Bool => BytecodeTypeKind::Bool,
            BytecodeType::UInt8 => BytecodeTypeKind::UInt8,
            BytecodeType::Char => BytecodeTypeKind::Char,
            BytecodeType::Int32 => BytecodeTypeKind::Int32,
            BytecodeType::Int64 => BytecodeTypeKind::Int64,
            BytecodeType::Float32 => BytecodeTypeKind::Float32,
            BytecodeType::Float64 => BytecodeTypeKind::Float64,
            BytecodeType::Ptr => BytecodeTypeKind::Ptr,
            BytecodeType::Tuple(_) => BytecodeTypeKind::Tuple,
            BytecodeType::TypeParam(_) => BytecodeTypeKind::TypeParam,
            BytecodeType::Enum(_, _) => BytecodeTypeKind::Enum,
            BytecodeType::Value(_, _) => BytecodeTypeKind::Value,
            BytecodeType::Class(_, _) => BytecodeTypeKind::Class,
            BytecodeType::Trait(_, _) => BytecodeTypeKind::Trait,
            BytecodeType::Lambda(_, _) => BytecodeTypeKind::Lambda,
        }
    }

    pub fn is_any_float(&self) -> bool {
        match self {
            BytecodeType::Float32 | BytecodeType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            BytecodeType::Ptr => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            BytecodeType::Unit => true,
            _ => false,
        }
    }

    pub fn is_class(&self) -> bool {
        match self {
            BytecodeType::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self {
            BytecodeType::Enum(_, _) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            BytecodeType::Value(_, _) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            BytecodeType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_trait(&self) -> bool {
        match self {
            BytecodeType::Trait(_, _) => true,
            _ => false,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match self {
            BytecodeType::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn tuple_subtypes(&self) -> SourceTypeArray {
        match self {
            BytecodeType::Tuple(subtypes) => subtypes.clone(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct BytecodeTypeArray(Arc<Vec<BytecodeType>>);
