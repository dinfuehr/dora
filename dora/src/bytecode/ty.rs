use crate::bytecode::BytecodeTypeKind;
use crate::language::sem_analysis::{ClassDefinitionId, EnumDefinitionId, StructDefinitionId};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::VM;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BytecodeType {
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
    Struct(StructDefinitionId, SourceTypeArray),
    Class(ClassDefinitionId, SourceTypeArray),
}

impl BytecodeType {
    pub fn kind(&self) -> BytecodeTypeKind {
        match self {
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
            BytecodeType::Struct(_, _) => BytecodeTypeKind::Struct,
            BytecodeType::Class(_, _) => BytecodeTypeKind::Class,
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

    pub fn is_type_param(&self) -> bool {
        match self {
            BytecodeType::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn from_ty(vm: &VM, ty: SourceType) -> BytecodeType {
        match ty {
            SourceType::Bool => BytecodeType::Bool,
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Char => BytecodeType::Char,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            SourceType::Class(_, _) => BytecodeType::Ptr,
            SourceType::Trait(_, _) => BytecodeType::Ptr,
            SourceType::Enum(id, params) => {
                let enum_ = vm.enums[id].read();

                for variant in &enum_.variants {
                    if !variant.types.is_empty() {
                        return BytecodeType::Enum(id, params);
                    }
                }

                BytecodeType::Int32
            }
            SourceType::Struct(id, params) => BytecodeType::Struct(id, params),
            SourceType::Tuple(subtypes) => BytecodeType::Tuple(subtypes),
            SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
            _ => panic!("BuiltinType {:?} cannot converted to BytecodeType", ty),
        }
    }

    pub fn tuple_subtypes(&self) -> SourceTypeArray {
        match self {
            BytecodeType::Tuple(subtypes) => subtypes.clone(),
            _ => unreachable!(),
        }
    }
}
