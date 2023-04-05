use parking_lot::RwLock;

use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::Span;

use crate::language::sem_analysis::{
    extension_matches_ty, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, SemAnalysis,
    SourceFileId, TraitDefinitionId, TypeParamDefinition,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplDefinitionId(pub u32);

impl ImplDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Id for ImplDefinition {
    type IdType = ImplDefinitionId;

    fn id_to_usize(id: ImplDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ImplDefinitionId {
        ImplDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut ImplDefinition, id: ImplDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ImplDefinition {
    pub id: Option<ImplDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub span: Span,
    pub type_params: Option<TypeParamDefinition>,
    pub trait_ty: SourceType,
    pub extended_ty: SourceType,
    pub methods: Vec<FctDefinitionId>,
    pub instance_names: HashMap<Name, FctDefinitionId>,
    pub static_names: HashMap<Name, FctDefinitionId>,
    pub impl_for: HashMap<FctDefinitionId, FctDefinitionId>,
}

impl ImplDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Impl>,
    ) -> ImplDefinition {
        ImplDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            type_params: None,
            span: node.span,
            trait_ty: SourceType::Error,
            extended_ty: SourceType::Error,
            methods: Vec::new(),
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
            impl_for: HashMap::new(),
        }
    }

    pub fn id(&self) -> ImplDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.as_ref().expect("uninitialized")
    }

    pub fn trait_id(&self) -> TraitDefinitionId {
        self.trait_ty.trait_id().expect("trait expected")
    }

    pub fn trait_ty(&self) -> SourceType {
        self.trait_ty.clone()
    }
}

impl Index<ImplDefinitionId> for Vec<RwLock<ImplDefinition>> {
    type Output = RwLock<ImplDefinition>;

    fn index(&self, index: ImplDefinitionId) -> &RwLock<ImplDefinition> {
        &self[index.0 as usize]
    }
}

pub fn impl_matches(
    sa: &SemAnalysis,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = sa.impls[impl_id].read();
    extension_matches_ty(
        sa,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty.clone(),
        impl_.type_params(),
    )
}

pub fn find_trait_impl(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    trait_ty: SourceType,
    object_type: SourceType,
) -> FctDefinitionId {
    debug_assert!(object_type.is_concrete_type());
    let impl_id = find_impl(
        sa,
        object_type,
        &TypeParamDefinition::new(),
        trait_ty.clone(),
    )
    .expect("no impl found for generic trait method call");

    let impl_ = sa.impls[impl_id].read();
    assert_eq!(
        impl_.trait_id(),
        trait_ty.trait_id().expect("trait expected")
    );

    impl_
        .impl_for
        .get(&fct_id)
        .cloned()
        .expect("no impl method found for generic trait call")
}

pub fn implements_trait(
    sa: &SemAnalysis,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
) -> bool {
    if check_ty.is_primitive()
        && sa.known.traits.zero() == trait_ty.trait_id().expect("trait expected")
    {
        assert!(trait_ty.type_params().is_empty());
        return true;
    }

    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => false,

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Struct(_, _)
        | SourceType::Enum(_, _)
        | SourceType::Class(_, _) => {
            find_impl(sa, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(tp_id, trait_ty),

        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn find_impl(
    sa: &SemAnalysis,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
) -> Option<ImplDefinitionId> {
    for impl_ in sa.impls.iter() {
        let impl_ = impl_.read();

        assert!(impl_.trait_ty().is_concrete_type());

        if impl_.extended_ty != check_ty {
            continue;
        }

        if impl_.trait_ty() != trait_ty {
            continue;
        }

        if impl_matches(sa, check_ty.clone(), check_type_param_defs, impl_.id()).is_some() {
            return Some(impl_.id());
        }
    }

    None
}
