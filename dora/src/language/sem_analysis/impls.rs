use parking_lot::RwLock;

use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{
    extension_matches_ty, FctDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId,
    TraitDefinitionId, TypeParamDefinition,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplDefinitionId(u32);

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
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub module_id: ModuleDefinitionId,
    pub pos: Position,
    pub type_params: TypeParamDefinition,
    pub trait_ty: SourceType,
    pub extended_ty: SourceType,
    pub methods: Vec<FctDefinitionId>,
    pub instance_names: HashMap<Name, FctDefinitionId>,
    pub static_names: HashMap<Name, FctDefinitionId>,
    pub impl_for: HashMap<FctDefinitionId, FctDefinitionId>,
}

impl ImplDefinition {
    pub fn new(
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        node: &Arc<ast::Impl>,
    ) -> ImplDefinition {
        ImplDefinition {
            id: None,
            file_id,
            ast: node.clone(),
            module_id,
            type_params: TypeParamDefinition::new_ast(&node.type_params),
            pos: node.pos,
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

    pub fn trait_id(&self) -> TraitDefinitionId {
        self.trait_ty.trait_id().expect("trait expected")
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
        &impl_.type_params,
    )
}

pub fn find_trait_impl(
    sa: &SemAnalysis,
    fct_id: FctDefinitionId,
    trait_ty: SourceType,
    object_type: SourceType,
) -> FctDefinitionId {
    debug_assert!(object_type.is_concrete_type(sa));
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
    let trait_id = trait_ty.trait_id().expect("trait expected");
    assert!(trait_ty.type_params().is_empty());

    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => false,

        SourceType::Enum(enum_id, _) => {
            let enum_ = sa.enums[enum_id].read();
            check_impls(sa, check_ty, check_type_param_defs, trait_ty, &enum_.impls).is_some()
        }

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => {
            if sa.known.traits.zero() == trait_id {
                return true;
            }

            let struct_id = check_ty
                .primitive_struct_id(sa)
                .expect("primitive expected");
            let xstruct = sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                sa,
                check_ty,
                check_type_param_defs,
                trait_ty,
                &xstruct.impls,
            )
            .is_some()
        }

        SourceType::Struct(struct_id, _) => {
            let xstruct = sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                sa,
                check_ty,
                check_type_param_defs,
                trait_ty,
                &xstruct.impls,
            )
            .is_some()
        }

        SourceType::Class(_, _) => {
            let cls_id = check_ty.cls_id().expect("class expected");
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            check_impls(
                sa,
                check_ty.clone(),
                check_type_param_defs,
                trait_ty,
                &cls.impls,
            )
            .is_some()
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
    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => None,

        SourceType::Enum(enum_id, _) => {
            let enum_ = sa.enums[enum_id].read();
            check_impls(sa, check_ty, check_type_param_defs, trait_ty, &enum_.impls)
        }

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => {
            let struct_id = check_ty
                .primitive_struct_id(sa)
                .expect("primitive expected");
            let xstruct = sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                sa,
                check_ty,
                check_type_param_defs,
                trait_ty,
                &xstruct.impls,
            )
        }

        SourceType::Struct(struct_id, _) => {
            let xstruct = sa.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                sa,
                check_ty,
                check_type_param_defs,
                trait_ty,
                &xstruct.impls,
            )
        }

        SourceType::Class(_, _) => {
            let cls_id = check_ty.cls_id().expect("class expected");
            let cls = sa.classes.idx(cls_id);
            let cls = cls.read();

            check_impls(
                sa,
                check_ty.clone(),
                check_type_param_defs,
                trait_ty,
                &cls.impls,
            )
        }

        SourceType::TypeParam(_) => unreachable!(),
        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn check_impls(
    sa: &SemAnalysis,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
    impls: &[ImplDefinitionId],
) -> Option<ImplDefinitionId> {
    let trait_id = trait_ty.trait_id().expect("trait expected");

    for &impl_id in impls {
        let impl_ = &sa.impls[impl_id];
        let impl_ = impl_.read();

        if impl_.trait_id() != trait_id {
            continue;
        }

        if impl_matches(sa, check_ty.clone(), check_type_param_defs, impl_id).is_some() {
            return Some(impl_id);
        }
    }

    None
}
