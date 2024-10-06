use std::ops::Index;
use std::sync::Arc;

use crate::sema::{
    AliasDefinitionId, ClassDefinition, ClassDefinitionId, ElementWithTypeParams, EnumDefinition,
    EnumDefinitionId, FctDefinition, Sema, StructDefinition, StructDefinitionId, TraitDefinitionId,
    TypeParamDefinition, TypeParamId,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SourceType {
    // couldn't determine type because of error
    Error,

    // Allow any type here, used for type inference
    Any,

    // type with only one value: ()
    Unit,

    // primitives
    Bool,
    Char,
    UInt8,
    Int32,
    Int64,
    Float32,
    Float64,

    // pointer to object, only used internally
    Ptr,

    // self type
    This,

    // some class
    Class(ClassDefinitionId, SourceTypeArray),

    // some struct
    Struct(StructDefinitionId, SourceTypeArray),

    // some tuple
    Tuple(SourceTypeArray),

    // some trait object
    Trait(TraitDefinitionId, SourceTypeArray),

    // some type variable
    TypeParam(TypeParamId),

    // Type alias.
    TypeAlias(AliasDefinitionId),

    // some lambda
    Lambda(SourceTypeArray, Box<SourceType>),

    // some enum
    Enum(EnumDefinitionId, SourceTypeArray),
}

impl SourceType {
    pub fn new_trait(trait_id: TraitDefinitionId) -> SourceType {
        SourceType::Trait(trait_id, SourceTypeArray::empty())
    }

    pub fn is_error(&self) -> bool {
        match self {
            SourceType::Error => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self {
            SourceType::Enum(_, _) => true,
            _ => false,
        }
    }

    pub fn is_enum_id(&self, enum_id: EnumDefinitionId) -> bool {
        match self {
            SourceType::Enum(id, _) => *id == enum_id,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            SourceType::Unit => true,
            _ => false,
        }
    }

    pub fn is_self(&self) -> bool {
        match self {
            SourceType::This => true,
            _ => false,
        }
    }

    pub fn is_cls(&self) -> bool {
        match self {
            SourceType::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_cls_id(&self, cls_id: ClassDefinitionId) -> bool {
        match self {
            SourceType::Class(id, _) => *id == cls_id,
            _ => false,
        }
    }

    pub fn is_trait(&self) -> bool {
        match self {
            SourceType::Trait(_, _) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            &SourceType::Float32 | &SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_int32(&self) -> bool {
        match self {
            &SourceType::Int32 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            &SourceType::Bool => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        match self {
            &SourceType::Char => true,
            _ => false,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match self {
            &SourceType::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            &SourceType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            &SourceType::Struct(_, _) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            &SourceType::Bool
            | &SourceType::UInt8
            | &SourceType::Char
            | &SourceType::Int32
            | &SourceType::Int64
            | &SourceType::Float32
            | &SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_tuple_or_unit(&self) -> bool {
        match self {
            &SourceType::Tuple(_) => true,
            &SourceType::Unit => true,
            _ => false,
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self {
            &SourceType::Lambda(_, _) => true,
            _ => false,
        }
    }

    pub fn cls_id(&self) -> Option<ClassDefinitionId> {
        match self {
            SourceType::Class(cls_id, _) => Some(*cls_id),
            _ => None,
        }
    }

    pub fn primitive_struct_id(&self, sa: &Sema) -> Option<StructDefinitionId> {
        match self {
            SourceType::Bool => Some(sa.known.structs.bool()),
            SourceType::UInt8 => Some(sa.known.structs.uint8()),
            SourceType::Char => Some(sa.known.structs.char()),
            SourceType::Int32 => Some(sa.known.structs.int32()),
            SourceType::Int64 => Some(sa.known.structs.int64()),
            SourceType::Float32 => Some(sa.known.structs.float32()),
            SourceType::Float64 => Some(sa.known.structs.float64()),
            _ => None,
        }
    }

    pub fn from_cls(cls_id: ClassDefinitionId) -> SourceType {
        SourceType::Class(cls_id, SourceTypeArray::empty())
    }

    pub fn enum_id(&self) -> Option<EnumDefinitionId> {
        match self {
            SourceType::Enum(enum_id, _) => Some(*enum_id),
            _ => None,
        }
    }

    pub fn struct_id(&self) -> Option<StructDefinitionId> {
        match self {
            SourceType::Struct(struct_id, _) => Some(*struct_id),
            _ => None,
        }
    }

    pub fn type_param_id(&self) -> Option<TypeParamId> {
        match self {
            SourceType::TypeParam(id) => Some(*id),
            _ => None,
        }
    }

    pub fn type_params(&self) -> SourceTypeArray {
        match self {
            SourceType::Class(_, params)
            | SourceType::Enum(_, params)
            | SourceType::Struct(_, params)
            | SourceType::Trait(_, params) => params.clone(),
            _ => SourceTypeArray::empty(),
        }
    }

    pub fn reference_type(&self) -> bool {
        match self {
            SourceType::Ptr => true,
            SourceType::Class(_, _) => true,
            SourceType::Trait(_, _) => true,
            SourceType::Lambda(_, _) => true,
            _ => false,
        }
    }

    pub fn value_type(&self) -> bool {
        match self {
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn name(&self, sa: &Sema) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: None,
        };

        writer.name(self.clone())
    }

    pub fn name_with_type_params(&self, sa: &Sema, type_params: &TypeParamDefinition) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: Some(type_params),
        };

        writer.name(self.clone())
    }

    pub fn name_fct(&self, sa: &Sema, fct: &FctDefinition) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: Some(fct.type_param_definition()),
        };

        writer.name(self.clone())
    }

    pub fn name_cls(&self, sa: &Sema, cls: &ClassDefinition) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: Some(cls.type_param_definition()),
        };

        writer.name(self.clone())
    }

    pub fn name_struct(&self, sa: &Sema, struct_: &StructDefinition) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: Some(struct_.type_param_definition()),
        };

        writer.name(self.clone())
    }

    pub fn name_enum(&self, sa: &Sema, enum_: &EnumDefinition) -> String {
        let writer = SourceTypePrinter {
            sa,
            type_params: Some(enum_.type_param_definition()),
        };

        writer.name(self.clone())
    }

    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        match self {
            SourceType::Trait(trait_id, _) => Some(*trait_id),
            _ => None,
        }
    }

    pub fn to_lambda(&self) -> Option<(SourceTypeArray, SourceType)> {
        match self {
            SourceType::Lambda(params, return_type) => {
                Some((params.clone(), return_type.as_ref().to_owned()))
            }
            _ => None,
        }
    }

    pub fn tuple_subtypes(&self) -> Option<SourceTypeArray> {
        match self {
            SourceType::Tuple(subtypes) => Some(subtypes.clone()),
            _ => None,
        }
    }

    pub fn allows(&self, sa: &Sema, other: SourceType) -> bool {
        match self {
            // allow all types for Error, there is already an error,
            // don't report too many messages for the same error
            SourceType::Error => true,

            // Any allows all other types
            SourceType::Any => true,

            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Struct(_, _)
            | SourceType::Enum(_, _)
            | SourceType::Trait(_, _) => *self == other,
            SourceType::Int32 | SourceType::Int64 | SourceType::Float32 | SourceType::Float64 => {
                *self == other
            }
            SourceType::Ptr => panic!("ptr does not allow any other types"),
            SourceType::This | SourceType::TypeAlias(..) => unreachable!(),
            SourceType::Class(self_cls_id, self_list) => {
                if *self == other {
                    return true;
                }

                let (other_cls_id, other_list) = match other {
                    SourceType::Class(cls_id, ref other_list) => (cls_id, other_list.clone()),
                    _ => {
                        return false;
                    }
                };

                *self_cls_id == other_cls_id && self_list == &other_list
            }
            SourceType::Tuple(subtypes) => match other {
                SourceType::Tuple(other_subtypes) => {
                    if subtypes.len() != other_subtypes.len() {
                        return false;
                    }

                    let len = subtypes.len();

                    for idx in 0..len {
                        let ty = subtypes[idx].clone();
                        let other_ty = other_subtypes[idx].clone();

                        if !ty.allows(sa, other_ty) {
                            return false;
                        }
                    }

                    true
                }

                _ => false,
            },

            SourceType::TypeParam(_) => *self == other,

            SourceType::Lambda(_, _) => {
                // for now expect the exact same params and return types
                // possible improvement: allow super classes for params,
                //                             sub class for return type
                *self == other
            }
        }
    }

    pub fn is_defined_type(&self, sa: &Sema) -> bool {
        match self {
            SourceType::Error | SourceType::This | SourceType::Any | SourceType::Ptr => false,
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Trait(_, _)
            | SourceType::Lambda(_, _)
            | SourceType::TypeParam(_) => true,
            SourceType::TypeAlias(..) => unreachable!(),
            SourceType::Enum(_, params)
            | SourceType::Class(_, params)
            | SourceType::Struct(_, params) => {
                for param in params.iter() {
                    if !param.is_defined_type(sa) {
                        return false;
                    }
                }

                true
            }
            SourceType::Tuple(subtypes) => {
                for ty in subtypes.iter() {
                    if !ty.is_defined_type(sa) {
                        return false;
                    }
                }

                true
            }
        }
    }

    pub fn is_concrete_type(&self) -> bool {
        match self {
            SourceType::Error | SourceType::This | SourceType::Any => false,
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Ptr => true,
            SourceType::TypeAlias(..) => unreachable!(),
            SourceType::Class(_, params)
            | SourceType::Enum(_, params)
            | SourceType::Struct(_, params)
            | SourceType::Trait(_, params) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }

                true
            }

            SourceType::Tuple(subtypes) => {
                for subtype in subtypes.iter() {
                    if !subtype.is_concrete_type() {
                        return false;
                    }
                }

                true
            }
            SourceType::Lambda(params, return_type) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }

                return_type.is_concrete_type()
            }
            SourceType::TypeParam(_) => false,
        }
    }
}

pub fn contains_self(sa: &Sema, ty: SourceType) -> bool {
    match ty {
        SourceType::Ptr | SourceType::Any => unreachable!(),
        SourceType::This => true,
        SourceType::Error
        | SourceType::Unit
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::TypeParam(..) => false,
        SourceType::TypeAlias(..) => unimplemented!(),
        SourceType::Class(_, params)
        | SourceType::Enum(_, params)
        | SourceType::Struct(_, params)
        | SourceType::Trait(_, params) => {
            for param in params.iter() {
                if contains_self(sa, param) {
                    return true;
                }
            }

            false
        }

        SourceType::Tuple(subtypes) => {
            for subtype in subtypes.iter() {
                if contains_self(sa, subtype) {
                    return true;
                }
            }

            false
        }
        SourceType::Lambda(params, return_type) => {
            for param in params.iter() {
                if contains_self(sa, param) {
                    return true;
                }
            }

            contains_self(sa, *return_type)
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SourceTypeArray {
    Empty,
    List(Arc<Vec<SourceType>>),
}

impl SourceTypeArray {
    pub fn empty() -> SourceTypeArray {
        SourceTypeArray::Empty
    }

    pub fn single(ty: SourceType) -> SourceTypeArray {
        SourceTypeArray::List(Arc::new(vec![ty]))
    }

    pub fn with(type_params: Vec<SourceType>) -> SourceTypeArray {
        if type_params.len() == 0 {
            SourceTypeArray::Empty
        } else {
            SourceTypeArray::List(Arc::new(type_params))
        }
    }

    pub fn new(types: Arc<Vec<SourceType>>) -> SourceTypeArray {
        SourceTypeArray::List(types)
    }

    pub fn connect(&self, other: &SourceTypeArray) -> SourceTypeArray {
        if self.is_empty() {
            return other.clone();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut params = self.types().to_vec();
        params.extend_from_slice(other.types());

        SourceTypeArray::List(Arc::new(params))
    }

    pub fn connect_single(&self, other: SourceType) -> SourceTypeArray {
        if self.is_empty() {
            return SourceTypeArray::single(other);
        }

        let mut params = self.types().to_vec();
        params.push(other);

        SourceTypeArray::List(Arc::new(params))
    }

    pub fn types(&self) -> &[SourceType] {
        match self {
            SourceTypeArray::Empty => &[],
            SourceTypeArray::List(ref params) => (**params).as_slice(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            &SourceTypeArray::Empty => 0,
            &SourceTypeArray::List(ref params) => params.len(),
        }
    }

    pub fn iter(&self) -> SourceTypeArrayIter {
        SourceTypeArrayIter {
            params: self,
            idx: 0,
        }
    }

    pub fn name(&self, sa: &Sema) -> String {
        let mut result = String::new();
        let mut first = true;
        result.push('[');

        for ty in self.iter() {
            if !first {
                result.push_str(", ");
            }
            result.push_str(&ty.name(sa));
            first = false;
        }

        result.push(']');

        result
    }

    pub fn tuple_name(&self, sa: &Sema) -> String {
        let mut result = String::new();
        let mut first = true;
        result.push('(');

        for ty in self.iter() {
            if !first {
                result.push_str(", ");
            }
            result.push_str(&ty.name(sa));
            first = false;
        }

        result.push(')');

        result
    }
}

impl Index<usize> for SourceTypeArray {
    type Output = SourceType;

    fn index(&self, idx: usize) -> &SourceType {
        match self {
            &SourceTypeArray::Empty => panic!("type list index out-of-bounds"),
            &SourceTypeArray::List(ref params) => &params[idx],
        }
    }
}

pub struct SourceTypeArrayIter<'a> {
    params: &'a SourceTypeArray,
    idx: usize,
}

impl<'a> Iterator for SourceTypeArrayIter<'a> {
    type Item = SourceType;

    fn next(&mut self) -> Option<SourceType> {
        match self.params {
            &SourceTypeArray::Empty => None,

            &SourceTypeArray::List(ref params) => {
                if self.idx < params.len() {
                    let ret = params[self.idx].clone();
                    self.idx += 1;

                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

struct SourceTypePrinter<'a> {
    sa: &'a Sema,
    type_params: Option<&'a TypeParamDefinition>,
}

impl<'a> SourceTypePrinter<'a> {
    pub fn name(&self, ty: SourceType) -> String {
        match ty {
            SourceType::Error => "<error>".into(),
            SourceType::Any => "Any".into(),
            SourceType::Unit => "()".into(),
            SourceType::UInt8 => "UInt8".into(),
            SourceType::Char => "Char".into(),
            SourceType::Int32 => "Int32".into(),
            SourceType::Int64 => "Int64".into(),
            SourceType::Float32 => "Float32".into(),
            SourceType::Float64 => "Float64".into(),
            SourceType::Bool => "Bool".into(),
            SourceType::Ptr => panic!("type Ptr only for internal use."),
            SourceType::This => "Self".into(),
            SourceType::Class(id, type_params) => {
                let cls = self.sa.class(id);
                let base = self.sa.interner.str(cls.name);

                if type_params.len() == 0 {
                    base.to_string()
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", base, params)
                }
            }
            SourceType::Struct(sid, type_params) => {
                let struct_ = self.sa.struct_(sid);
                let name = struct_.name;
                let name = self.sa.interner.str(name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            SourceType::Trait(trait_id, type_params) => {
                let trait_ = self.sa.trait_(trait_id);
                let name = self.sa.interner.str(trait_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            SourceType::Enum(id, type_params) => {
                let enum_ = self.sa.enum_(id);
                let name = self.sa.interner.str(enum_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }

            SourceType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    self.sa.interner.str(type_params.name(idx)).to_string()
                } else {
                    format!("TypeParam({})", idx.index())
                }
            }

            SourceType::Lambda(params, return_type) => {
                let params = params
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.name(*return_type);

                format!("({}) -> {}", params, ret)
            }

            SourceType::Tuple(subtypes) => {
                let types = subtypes
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }

            SourceType::TypeAlias(id) => {
                let alias = self.sa.alias(id);
                format!("{}", self.sa.interner.str(alias.name))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitType {
    pub trait_id: TraitDefinitionId,
    pub type_params: SourceTypeArray,
    pub bindings: Vec<(AliasDefinitionId, SourceType)>,
}

impl TraitType {
    pub fn new_ty(sa: &Sema, ty: SourceType) -> TraitType {
        match ty {
            SourceType::Trait(trait_id, type_params) => {
                let trait_ = sa.trait_(trait_id);
                let type_params = type_params.types();
                let generic_count = trait_.type_param_definition().type_param_count();
                let generic_args = &type_params[0..generic_count];
                let type_bindings = &type_params[generic_count..];

                assert_eq!(trait_.aliases().len(), type_bindings.len());

                let new_bindings = type_bindings
                    .iter()
                    .enumerate()
                    .map(|(idx, ty)| (trait_.aliases()[idx], ty.clone()))
                    .collect();

                let trait_type = TraitType {
                    trait_id,
                    type_params: SourceTypeArray::with(generic_args.to_vec()),
                    bindings: new_bindings,
                };

                trait_type
            }

            _ => unreachable!(),
        }
    }

    pub fn from_trait_id(id: TraitDefinitionId) -> TraitType {
        TraitType {
            trait_id: id,
            type_params: SourceTypeArray::empty(),
            bindings: Vec::new(),
        }
    }

    pub fn ty(&self) -> SourceType {
        SourceType::Trait(self.trait_id, self.type_params.clone())
    }

    pub fn name_with_type_params(
        &self,
        sa: &Sema,
        type_param_definition: &TypeParamDefinition,
    ) -> String {
        let trait_ = sa.trait_(self.trait_id);
        let name = sa.interner.str(trait_.name).to_string();

        if self.type_params.is_empty() {
            name
        } else {
            let writer = SourceTypePrinter {
                sa,
                type_params: Some(type_param_definition),
            };

            let params = self
                .type_params
                .iter()
                .map(|ty| writer.name(ty))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, params)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_type_lists() {
        let e1 = SourceTypeArray::empty();
        let e2 = SourceTypeArray::single(SourceType::Int32);
        assert_eq!(e1.connect(&e2).types(), &[SourceType::Int32]);

        let e1 = SourceTypeArray::single(SourceType::Float32);
        let e2 = SourceTypeArray::single(SourceType::Int32);
        assert_eq!(
            e1.connect(&e2).types(),
            &[SourceType::Float32, SourceType::Int32]
        );
    }
}
