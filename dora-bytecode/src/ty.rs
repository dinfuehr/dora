use bincode::{Decode, Encode};
use std::ops::Index;
use std::sync::Arc;

use crate::{BytecodeTypeKind, ClassId, EnumId, StructId, TraitId, program::AliasId};

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
    TraitObject(TraitId, BytecodeTypeArray, BytecodeTypeArray),
    Lambda(BytecodeTypeArray, Box<BytecodeType>),
    TypeAlias(AliasId),
    Assoc {
        ty: Box<BytecodeType>,
        trait_ty: BytecodeTraitType,
        assoc_id: AliasId,
    },
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
            BytecodeType::TraitObject(..) => BytecodeTypeKind::TraitObject,
            BytecodeType::Lambda(..) => BytecodeTypeKind::Lambda,
            BytecodeType::TypeAlias(..) => BytecodeTypeKind::TypeAlias,
            BytecodeType::Assoc { .. } => BytecodeTypeKind::Assoc,
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

    pub fn is_trait_object(&self) -> bool {
        match self {
            BytecodeType::TraitObject(..) => true,
            _ => false,
        }
    }

    pub fn to_trait_id(&self) -> Option<TraitId> {
        match self {
            BytecodeType::TraitObject(id, ..) => Some(*id),
            _ => None,
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

    pub fn is_assoc(&self) -> bool {
        match self {
            BytecodeType::Assoc { .. } => true,
            _ => false,
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
            | BytecodeType::Struct(_, params) => params.is_concrete_type(),

            BytecodeType::TraitObject(_, params, bindings) => {
                params.is_concrete_type() && bindings.is_concrete_type()
            }

            BytecodeType::Tuple(subtypes) => subtypes.is_concrete_type(),
            BytecodeType::Lambda(params, return_type) => {
                params.is_concrete_type() && return_type.is_concrete_type()
            }
            BytecodeType::TypeParam(_) => false,
            BytecodeType::TypeAlias(..) | BytecodeType::Assoc { .. } | BytecodeType::This => {
                unreachable!()
            }
        }
    }

    pub fn is_reference_type(&self) -> bool {
        match self {
            BytecodeType::Ptr => true,
            BytecodeType::Class(..) => true,
            BytecodeType::TraitObject(..) => true,
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

    pub fn iter(&self) -> BytecodeTypeArrayIter<'_> {
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

    pub fn connect(&self, rhs: &BytecodeTypeArray) -> BytecodeTypeArray {
        let mut types = self.0.to_vec();
        types.append(&mut rhs.to_vec());
        BytecodeTypeArray::new(types)
    }

    pub fn split(&self, elements: usize) -> (BytecodeTypeArray, BytecodeTypeArray) {
        let (first, second) = self.0.split_at(elements);

        (
            BytecodeTypeArray::new(first.to_vec()),
            BytecodeTypeArray::new(second.to_vec()),
        )
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

#[derive(Clone, Debug, Decode, Encode, PartialEq, Eq, Hash)]
pub struct BytecodeTraitType {
    pub trait_id: TraitId,
    pub type_params: BytecodeTypeArray,
    pub bindings: Vec<(AliasId, BytecodeType)>,
}

impl BytecodeTraitType {
    pub fn is_trait_object_ty(&self, ty: &BytecodeType) -> bool {
        match ty {
            BytecodeType::TraitObject(trait_id, type_params, bindings) => {
                assert!(bindings.is_empty());
                self.trait_id == *trait_id && &self.type_params == type_params
            }

            _ => false,
        }
    }
}
