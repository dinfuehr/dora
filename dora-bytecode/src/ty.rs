use bincode::{Decode, Encode};
use std::ops::Index;
use std::sync::Arc;

use crate::{program::AliasId, BytecodeTypeKind, ClassId, EnumId, StructId, TraitId};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
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
    This,
    Tuple(BytecodeTypeArray),
    TypeParam(u32),
    Enum(EnumId, BytecodeTypeArray),
    Struct(StructId, BytecodeTypeArray),
    Class(ClassId, BytecodeTypeArray),
    Trait(TraitId, BytecodeTypeArray),
    Lambda(BytecodeTypeArray, Box<BytecodeType>),
    TypeAlias(AliasId),
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
            BytecodeType::Tuple(..) => BytecodeTypeKind::Tuple,
            BytecodeType::TypeParam(..) => BytecodeTypeKind::TypeParam,
            BytecodeType::Enum(..) => BytecodeTypeKind::Enum,
            BytecodeType::Struct(..) => BytecodeTypeKind::Struct,
            BytecodeType::Class(..) => BytecodeTypeKind::Class,
            BytecodeType::Trait(..) => BytecodeTypeKind::Trait,
            BytecodeType::Lambda(..) => BytecodeTypeKind::Lambda,
            BytecodeType::TypeAlias(..) => BytecodeTypeKind::TypeAlias,
            BytecodeType::This => unreachable!(),
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
            BytecodeType::Struct(_, _) => true,
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

    pub fn tuple_subtypes(&self) -> BytecodeTypeArray {
        match self {
            BytecodeType::Tuple(subtypes) => subtypes.clone(),
            _ => unreachable!(),
        }
    }

    pub fn is_concrete_type(&self) -> bool {
        match self {
            BytecodeType::Unit
            | BytecodeType::Bool
            | BytecodeType::UInt8
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Ptr => true,
            BytecodeType::Class(_, params)
            | BytecodeType::Enum(_, params)
            | BytecodeType::Struct(_, params)
            | BytecodeType::Trait(_, params) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }

                true
            }

            BytecodeType::Tuple(subtypes) => {
                for subtype in subtypes.iter() {
                    if !subtype.is_concrete_type() {
                        return false;
                    }
                }

                true
            }
            BytecodeType::Lambda(params, return_type) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }

                return_type.is_concrete_type()
            }
            BytecodeType::TypeParam(_) => false,
            BytecodeType::TypeAlias(..) | BytecodeType::This => unreachable!(),
        }
    }

    pub fn is_reference_type(&self) -> bool {
        match self {
            BytecodeType::Ptr => true,
            BytecodeType::Class(..) => true,
            BytecodeType::Trait(..) => true,
            BytecodeType::Lambda(..) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct BytecodeTypeArray(Arc<Vec<BytecodeType>>);

impl BytecodeTypeArray {
    pub fn new(types: Vec<BytecodeType>) -> BytecodeTypeArray {
        BytecodeTypeArray(Arc::new(types))
    }

    pub fn one(ty: BytecodeType) -> BytecodeTypeArray {
        BytecodeTypeArray(Arc::new(vec![ty]))
    }

    pub fn empty() -> BytecodeTypeArray {
        BytecodeTypeArray(Arc::new(Vec::new()))
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> BytecodeTypeArrayIter {
        BytecodeTypeArrayIter {
            params: self,
            idx: 0,
        }
    }

    pub fn append(&self, ty: BytecodeType) -> BytecodeTypeArray {
        let mut types = (*self.0).to_owned();
        types.push(ty);
        BytecodeTypeArray::new(types)
    }

    pub fn is_concrete_type(&self) -> bool {
        self.0.iter().all(|ty| ty.is_concrete_type())
    }

    pub fn to_vec(&self) -> Vec<BytecodeType> {
        (*self.0).clone()
    }
}

impl Index<usize> for BytecodeTypeArray {
    type Output = BytecodeType;

    fn index(&self, idx: usize) -> &BytecodeType {
        &self.0[idx]
    }
}

pub struct BytecodeTypeArrayIter<'a> {
    params: &'a BytecodeTypeArray,
    idx: usize,
}

impl<'a> Iterator for BytecodeTypeArrayIter<'a> {
    type Item = BytecodeType;

    fn next(&mut self) -> Option<BytecodeType> {
        if self.idx < self.params.len() {
            let ret = self.params[self.idx].clone();
            self.idx += 1;

            Some(ret)
        } else {
            None
        }
    }
}
