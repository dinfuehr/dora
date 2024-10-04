use std::cell::OnceCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    extension_matches_ty, AliasDefinitionId, FctDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, Sema, SourceFileId, TraitDefinitionId, TypeParamDefinition,
};
use crate::ty::{SourceType, SourceTypeArray};
use crate::{ParsedTraitType, ParsedType, TraitType};
use id_arena::Id;

pub type ImplDefinitionId = Id<ImplDefinition>;

#[derive(Debug)]
pub struct ImplDefinition {
    pub id: OnceCell<ImplDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub declaration_span: Span,
    pub span: Span,
    pub type_param_definition: Rc<TypeParamDefinition>,
    pub parsed_trait_ty: ParsedTraitType,
    pub parsed_extended_ty: ParsedType,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub aliases: OnceCell<Vec<AliasDefinitionId>>,
    pub trait_method_map: OnceCell<HashMap<FctDefinitionId, FctDefinitionId>>,
    pub trait_alias_map: OnceCell<HashMap<AliasDefinitionId, AliasDefinitionId>>,
}

impl ImplDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Impl>,
        type_param_definition: Rc<TypeParamDefinition>,
    ) -> ImplDefinition {
        ImplDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            type_param_definition,
            declaration_span: node.declaration_span,
            span: node.span,
            parsed_trait_ty: ParsedTraitType::new_ast(
                node.trait_type
                    .as_ref()
                    .expect("missing trait type")
                    .clone(),
            ),
            parsed_extended_ty: ParsedType::new_ast(node.extended_type.clone()),
            methods: OnceCell::new(),
            aliases: OnceCell::new(),
            trait_method_map: OnceCell::new(),
            trait_alias_map: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ImplDefinitionId {
        self.id.get().expect("id missing").clone()
    }

    pub fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        &self.type_param_definition
    }

    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        self.trait_ty().map(|t| t.trait_id)
    }

    pub fn trait_ty(&self) -> Option<TraitType> {
        self.parsed_trait_ty().ty()
    }

    pub fn parsed_trait_ty(&self) -> &ParsedTraitType {
        &self.parsed_trait_ty
    }

    pub fn extended_ty(&self) -> SourceType {
        self.parsed_extended_ty().ty()
    }

    pub fn parsed_extended_ty(&self) -> &ParsedType {
        &self.parsed_extended_ty
    }

    pub fn trait_method_map(&self) -> &HashMap<FctDefinitionId, FctDefinitionId> {
        self.trait_method_map
            .get()
            .expect("missing trait_method_map")
    }

    pub fn trait_alias_map(&self) -> &HashMap<AliasDefinitionId, AliasDefinitionId> {
        self.trait_alias_map.get().expect("missing trait_alias_map")
    }

    pub fn get_method_for_trait_method_id(
        &self,
        trait_method_id: FctDefinitionId,
    ) -> Option<FctDefinitionId> {
        self.trait_method_map().get(&trait_method_id).cloned()
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("missing methods")
    }

    pub fn aliases(&self) -> &[AliasDefinitionId] {
        self.aliases.get().expect("missing methods")
    }
}

pub fn impl_matches(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = sa.impl_(impl_id);
    extension_matches_ty(
        sa,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty(),
        impl_.type_param_definition(),
    )
}

pub fn implements_trait(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: TraitType,
) -> bool {
    let check_ty = maybe_alias_ty(sa, check_ty);

    if check_ty.is_primitive() && sa.known.traits.zero() == trait_ty.trait_id {
        assert!(trait_ty.type_params.is_empty());
        return true;
    }

    match check_ty {
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
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => {
            find_impl(sa, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(tp_id, trait_ty),

        SourceType::TypeAlias(..) => unreachable!(),

        SourceType::Error => false,

        SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn maybe_alias_ty(sa: &Sema, mut ty: SourceType) -> SourceType {
    while let SourceType::TypeAlias(id) = ty {
        let alias = sa.alias(id);
        ty = alias.ty();
    }

    ty
}

pub struct ImplMatch {
    pub id: ImplDefinitionId,
    pub binding: SourceTypeArray,
}

pub fn find_impl(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: TraitType,
) -> Option<ImplMatch> {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(impl_trait_ty) = impl_.trait_ty() {
            if !trait_ty_match(sa, impl_, &impl_trait_ty, &trait_ty) {
                continue;
            }

            if let Some(binding) =
                impl_matches(sa, check_ty.clone(), check_type_param_defs, impl_.id())
            {
                return Some(ImplMatch {
                    id: impl_.id(),
                    binding,
                });
            }
        }
    }

    None
}

fn trait_ty_match(
    sa: &Sema,
    impl_: &ImplDefinition,
    impl_trait_ty: &TraitType,
    trait_ty: &TraitType,
) -> bool {
    if impl_trait_ty.trait_id != trait_ty.trait_id
        || impl_trait_ty.type_params != trait_ty.type_params
    {
        return false;
    }

    let trait_alias_map = impl_.trait_alias_map();

    for (trait_alias_id, type_binding) in &trait_ty.bindings {
        let impl_alias_id = trait_alias_map.get(&trait_alias_id).expect("missing alias");
        let impl_alias_ty = sa.alias(*impl_alias_id).ty();

        if type_binding != &impl_alias_ty {
            return false;
        }
    }

    true
}
