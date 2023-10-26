use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::Name;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    extension_matches_ty, AliasDefinitionId, FctDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TraitDefinitionId, TypeParamDefinition,
};
use crate::ty::{SourceType, SourceTypeArray};
use id_arena::Id;

pub type ImplDefinitionId = Id<ImplDefinition>;

#[derive(Debug)]
pub struct ImplDefinition {
    pub id: OnceCell<ImplDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub span: Span,
    pub type_params: OnceCell<TypeParamDefinition>,
    pub trait_ty: OnceCell<SourceType>,
    pub extended_ty: OnceCell<SourceType>,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub aliases: OnceCell<Vec<AliasDefinitionId>>,
    pub instance_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub impl_for: OnceCell<HashMap<FctDefinitionId, FctDefinitionId>>,
}

impl ImplDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Impl>,
    ) -> ImplDefinition {
        ImplDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            type_params: OnceCell::new(),
            span: node.span,
            trait_ty: OnceCell::new(),
            extended_ty: OnceCell::new(),
            methods: OnceCell::new(),
            aliases: OnceCell::new(),
            instance_names: RefCell::new(HashMap::new()),
            static_names: RefCell::new(HashMap::new()),
            impl_for: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ImplDefinitionId {
        self.id.get().expect("id missing").clone()
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.get().expect("uninitialized")
    }

    pub fn trait_id(&self) -> TraitDefinitionId {
        self.trait_ty().trait_id().expect("trait expected")
    }

    pub fn trait_ty(&self) -> SourceType {
        self.trait_ty.get().expect("missing trait type").clone()
    }

    pub fn extended_ty(&self) -> SourceType {
        self.extended_ty.get().expect("missing trait type").clone()
    }

    pub fn impl_for(&self) -> &HashMap<FctDefinitionId, FctDefinitionId> {
        self.impl_for.get().expect("missing impl")
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("missing methods")
    }
}

pub fn impl_matches(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = &sa.impls[impl_id];
    extension_matches_ty(
        sa,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty(),
        impl_.type_params(),
    )
}

pub fn implements_trait(
    sa: &Sema,
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
        SourceType::Trait(_, _) | SourceType::Lambda(_, _) => false,

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Struct(_, _)
        | SourceType::Enum(_, _)
        | SourceType::Class(_, _)
        | SourceType::Tuple(_)
        | SourceType::Unit => find_impl(sa, check_ty, check_type_param_defs, trait_ty).is_some(),

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(tp_id, trait_ty),

        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn find_impl(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
) -> Option<ImplDefinitionId> {
    for (_id, impl_) in sa.impls.iter() {
        assert!(impl_.trait_ty().is_concrete_type());

        if impl_.trait_ty() != trait_ty {
            continue;
        }

        if impl_matches(sa, check_ty.clone(), check_type_param_defs, impl_.id()).is_some() {
            return Some(impl_.id());
        }
    }

    None
}
