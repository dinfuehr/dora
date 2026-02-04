use std::convert::From;
use std::ops::Index;
use std::sync::Arc;

use crate::sema::{
    AliasDefinitionId, ClassDefinition, ClassDefinitionId, Element, EnumDefinition,
    EnumDefinitionId, FctDefinition, Sema, StructDefinition, StructDefinitionId, TraitDefinitionId,
    TypeParamDefinition, TypeParamId,
};
use crate::specialize::specialize_trait_type_for_implements;

pub fn error() -> SourceType {
    SourceType::Error
}

pub fn any() -> SourceType {
    SourceType::Any
}

pub fn unit() -> SourceType {
    SourceType::Unit
}

pub fn bool() -> SourceType {
    SourceType::Bool
}

pub fn char() -> SourceType {
    SourceType::Bool
}

pub fn uint8() -> SourceType {
    SourceType::UInt8
}

pub fn int32() -> SourceType {
    SourceType::Int32
}

pub fn int64() -> SourceType {
    SourceType::Int32
}

pub fn float32() -> SourceType {
    SourceType::Int32
}

pub fn float64() -> SourceType {
    SourceType::Int32
}

pub fn ptr() -> SourceType {
    SourceType::Ptr
}

pub fn self_() -> SourceType {
    SourceType::This
}

pub fn class(id: ClassDefinitionId, type_params: SourceTypeArray) -> SourceType {
    SourceType::Class(id, type_params)
}

pub fn struct_(id: StructDefinitionId, type_params: SourceTypeArray) -> SourceType {
    SourceType::Struct(id, type_params)
}

pub fn enum_(id: EnumDefinitionId, type_params: SourceTypeArray) -> SourceType {
    SourceType::Enum(id, type_params)
}

pub fn tuple(types: SourceTypeArray) -> SourceType {
    SourceType::Tuple(types)
}

pub fn trait_(id: TraitDefinitionId, type_params: SourceTypeArray) -> SourceType {
    SourceType::TraitObject(id, type_params, ().into())
}

pub fn type_param(id: TypeParamId) -> SourceType {
    SourceType::TypeParam(id)
}

pub fn alias(id: AliasDefinitionId, type_params: SourceTypeArray) -> SourceType {
    SourceType::Alias(id, type_params)
}

pub fn lambda(type_params: SourceTypeArray, return_type: SourceType) -> SourceType {
    SourceType::Lambda(type_params, Box::new(return_type))
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TyKind {
    Error,
    Any,
    Ptr,
    This,
    Class,
    Struct,
    Tuple,
    TraitObject,
    TypeParam,
    Alias,
    Assoc,
    GenericAssoc,
    Lambda,
    Enum,
    Ref,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SourceType {
    // Couldn't determine type because of error.
    Error,

    // Allow any type here, used for type inference
    Any,

    // Type with only one value: ().
    Unit,

    // Primitives
    Bool,
    Char,
    UInt8,
    Int32,
    Int64,
    Float32,
    Float64,

    // Pointer to object, only used internally.
    Ptr,

    // Self type.
    This,

    // Some class.
    Class(ClassDefinitionId, SourceTypeArray),

    // Some struct.
    Struct(StructDefinitionId, SourceTypeArray),

    // Some tuple.
    Tuple(SourceTypeArray),

    // Some trait object.
    TraitObject(TraitDefinitionId, SourceTypeArray, SourceTypeArray),

    // Some type variable.
    TypeParam(TypeParamId),

    // Type alias.
    Alias(AliasDefinitionId, SourceTypeArray),

    // Some associated type (used through Self::X).
    Assoc {
        trait_ty: TraitType,
        assoc_id: AliasDefinitionId,
    },

    // Some associated type on a type (T::X or [T as Trait]::X).
    GenericAssoc {
        ty: Box<SourceType>,
        trait_ty: TraitType,
        assoc_id: AliasDefinitionId,
    },

    // Some lambda.
    Lambda(SourceTypeArray, Box<SourceType>),

    // Some enum.
    Enum(EnumDefinitionId, SourceTypeArray),

    // Reference to a value type (used for self in mutating methods).
    Ref(Box<SourceType>),
}

impl SourceType {
    pub fn kind(&self) -> TyKind {
        match self {
            SourceType::Error => TyKind::Error,
            SourceType::This => TyKind::This,
            SourceType::Any => TyKind::Any,
            SourceType::Ptr => TyKind::Ptr,
            SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Struct(..) => TyKind::Struct,
            SourceType::TraitObject(..) => TyKind::TraitObject,
            SourceType::Lambda(..) => TyKind::Lambda,
            SourceType::TypeParam(..) => TyKind::TypeParam,
            SourceType::Alias(..) => TyKind::Alias,
            SourceType::Enum(..) => TyKind::Enum,
            SourceType::Class(..) => TyKind::Class,
            SourceType::Unit | SourceType::Tuple(..) => TyKind::Tuple,
            SourceType::Assoc { .. } => TyKind::Assoc,
            SourceType::GenericAssoc { .. } => TyKind::GenericAssoc,
            SourceType::Ref(..) => TyKind::Ref,
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            SourceType::Error => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match self {
            SourceType::Enum(..) => true,
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

    pub fn is_assoc(&self) -> bool {
        match self {
            SourceType::Assoc { .. } => true,
            _ => false,
        }
    }

    pub fn is_generic_assoc(&self) -> bool {
        match self {
            SourceType::GenericAssoc { .. } => true,
            _ => false,
        }
    }

    pub fn is_class(&self) -> bool {
        match self {
            SourceType::Class(..) => true,
            _ => false,
        }
    }

    pub fn is_trait_object(&self) -> bool {
        match self {
            SourceType::TraitObject(..) => true,
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

    pub fn to_class(&self) -> Option<(ClassDefinitionId, SourceTypeArray)> {
        match self {
            SourceType::Class(id, types) => Some((*id, types.clone())),
            _ => None,
        }
    }

    pub fn enum_id(&self) -> Option<EnumDefinitionId> {
        match self {
            SourceType::Enum(enum_id, _) => Some(*enum_id),
            _ => None,
        }
    }

    pub fn to_enum(&self) -> Option<(EnumDefinitionId, SourceTypeArray)> {
        match self {
            SourceType::Enum(id, types) => Some((*id, types.clone())),
            _ => None,
        }
    }

    pub fn struct_id(&self) -> Option<StructDefinitionId> {
        match self {
            SourceType::Struct(struct_id, _) => Some(*struct_id),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<(StructDefinitionId, SourceTypeArray)> {
        match self {
            SourceType::Struct(id, types) => Some((*id, types.clone())),
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
            | SourceType::Struct(_, params) => params.clone(),
            SourceType::TraitObject(..) => unimplemented!(),
            _ => SourceTypeArray::empty(),
        }
    }

    pub fn reference_type(&self) -> bool {
        match self {
            SourceType::Ptr => true,
            SourceType::Class(..) => true,
            SourceType::TraitObject(..) => true,
            SourceType::Lambda(..) => true,
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
            SourceType::TraitObject(trait_id, ..) => Some(*trait_id),
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
        if other.is_error() {
            return true;
        }

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
            | SourceType::Struct(..)
            | SourceType::Enum(..)
            | SourceType::TraitObject(..)
            | SourceType::Class(..)
            | SourceType::Alias(..)
            | SourceType::Assoc { .. }
            | SourceType::This
            | SourceType::TypeParam(..)
            | SourceType::Lambda(..)
            | SourceType::GenericAssoc { .. }
            | SourceType::Ref(..) => *self == other,
            SourceType::Int32 | SourceType::Int64 | SourceType::Float32 | SourceType::Float64 => {
                *self == other
            }
            SourceType::Ptr => panic!("ptr does not allow any other types"),
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
            | SourceType::TraitObject(..)
            | SourceType::Lambda(..)
            | SourceType::TypeParam(_) => true,
            SourceType::Assoc { trait_ty, .. } | SourceType::GenericAssoc { trait_ty, .. } => {
                for ty in trait_ty.type_params.iter() {
                    if !ty.is_defined_type(sa) {
                        return false;
                    }
                }

                for (_, ty) in trait_ty.bindings.iter() {
                    if !ty.is_defined_type(sa) {
                        return false;
                    }
                }

                true
            }
            SourceType::Alias(..) | SourceType::Ref(..) => {
                unreachable!()
            }
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
            SourceType::Class(_, params)
            | SourceType::Enum(_, params)
            | SourceType::Struct(_, params) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }

                true
            }

            SourceType::TraitObject(_, params, bindings) => {
                for param in params.iter() {
                    if !param.is_concrete_type() {
                        return false;
                    }
                }
                for param in bindings.iter() {
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
            SourceType::Alias(..)
            | SourceType::TypeParam(_)
            | SourceType::Assoc { .. }
            | SourceType::GenericAssoc { .. }
            | SourceType::Ref(..) => false,
        }
    }
}

pub fn contains_self(sa: &Sema, ty: SourceType) -> bool {
    match ty {
        SourceType::Ptr | SourceType::Any | SourceType::Ref(..) => unreachable!(),
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
        | SourceType::TypeParam(..)
        | SourceType::Assoc { .. } => false,

        SourceType::Alias(..) | SourceType::GenericAssoc { .. } => {
            unimplemented!()
        }
        SourceType::Class(_, params)
        | SourceType::Enum(_, params)
        | SourceType::Struct(_, params) => {
            for param in params.iter() {
                if contains_self(sa, param) {
                    return true;
                }
            }

            false
        }

        SourceType::TraitObject(_, type_params, bindings) => {
            for param in type_params.iter() {
                if contains_self(sa, param) {
                    return true;
                }
            }

            for param in bindings.iter() {
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

pub fn empty_sta() -> SourceTypeArray {
    SourceTypeArray::Empty
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
            SourceTypeArray::List(params) => (**params).as_slice(),
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

    pub fn iter(&self) -> SourceTypeArrayIter<'_> {
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

impl From<Vec<SourceType>> for SourceTypeArray {
    fn from(value: Vec<SourceType>) -> Self {
        SourceTypeArray::with(value)
    }
}

impl From<SourceType> for SourceTypeArray {
    fn from(value: SourceType) -> Self {
        SourceTypeArray::single(value)
    }
}

impl From<()> for SourceTypeArray {
    fn from(_value: ()) -> Self {
        SourceTypeArray::empty()
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
            SourceType::TraitObject(trait_id, type_params, bindings) => {
                let trait_ = self.sa.trait_(trait_id);
                let name = self.sa.interner.str(trait_.name).to_string();

                if type_params.is_empty() && bindings.is_empty() {
                    name
                } else {
                    let mut params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    for (idx, binding) in bindings.iter().enumerate() {
                        if !params.is_empty() {
                            params.push_str(", ");
                        }

                        let alias_id = trait_.aliases()[idx];
                        let alias = self.sa.alias(alias_id);
                        params.push_str(&self.sa.name(alias.name));
                        params.push_str(" = ");
                        params.push_str(&self.name(binding));
                    }

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

            SourceType::Alias(id, type_params) => {
                let alias = self.sa.alias(id);
                let name = self.sa.interner.str(alias.name).to_string();

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

            SourceType::Assoc { assoc_id, .. } => {
                let alias = self.sa.alias(assoc_id);
                let name = self.sa.interner.str(alias.name).to_string();

                format!("Self::{}", name)
            }

            SourceType::GenericAssoc {
                ty,
                trait_ty,
                assoc_id,
            } => {
                let ty_name = SourceTypePrinter {
                    sa: self.sa,
                    type_params: self.type_params,
                }
                .name(ty.as_ref().clone());

                let trait_ = self.sa.trait_(trait_ty.trait_id);
                let trait_name = self.sa.interner.str(trait_.name);

                let alias = self.sa.alias(assoc_id);
                let alias_name = self.sa.interner.str(alias.name);

                format!("[{} as {}]::{}", ty_name, trait_name, alias_name)
            }

            SourceType::Ref(inner) => {
                format!("ref {}", self.name(inner.as_ref().clone()))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitType {
    pub trait_id: TraitDefinitionId,
    pub type_params: SourceTypeArray,
    pub bindings: Vec<(AliasDefinitionId, SourceType)>,
}

impl TraitType {
    pub fn new_ty(sa: &Sema, ty: SourceType) -> TraitType {
        match ty {
            SourceType::TraitObject(trait_id, type_params, bindings) => {
                let trait_ = sa.trait_(trait_id);

                assert_eq!(trait_.aliases().len(), bindings.len());

                let new_bindings = bindings
                    .iter()
                    .enumerate()
                    .map(|(idx, ty)| (trait_.aliases()[idx], ty.clone()))
                    .collect();

                let trait_type = TraitType {
                    trait_id,
                    type_params,
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
        assert!(self.bindings.is_empty());
        SourceType::TraitObject(self.trait_id, self.type_params.clone(), ().into())
    }

    pub fn implements_trait(&self, sa: &Sema, check_trait_ty: &TraitType) -> bool {
        if self == check_trait_ty {
            return true;
        }

        let trait_ = sa.trait_(self.trait_id);

        for super_trait_ty in trait_.type_param_definition().bounds_for_self() {
            // Specialize the super trait's type params with this trait's type params
            let specialized_super_trait_ty =
                specialize_trait_type_for_implements(super_trait_ty, &self.type_params);
            if specialized_super_trait_ty.implements_trait(sa, check_trait_ty) {
                return true;
            }
        }

        false
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
