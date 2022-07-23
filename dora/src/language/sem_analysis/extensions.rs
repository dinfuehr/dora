use parking_lot::RwLock;

use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use crate::language::sem_analysis::{
    FctDefinitionId, ModuleDefinitionId, SourceFileId, TypeParamDefinition,
};
use crate::language::ty::SourceType;
use crate::utils::Id;

pub use self::matching::{extension_matches, extension_matches_ty};
use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExtensionDefinitionId(u32);

impl ExtensionDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl Id for ExtensionDefinition {
    type IdType = ExtensionDefinitionId;

    fn id_to_usize(id: ExtensionDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> ExtensionDefinitionId {
        ExtensionDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut ExtensionDefinition, id: ExtensionDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct ExtensionDefinition {
    pub id: Option<ExtensionDefinitionId>,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub module_id: ModuleDefinitionId,
    pub pos: Position,
    pub type_params: Option<TypeParamDefinition>,
    pub ty: SourceType,
    pub methods: Vec<FctDefinitionId>,
    pub instance_names: HashMap<Name, FctDefinitionId>,
    pub static_names: HashMap<Name, FctDefinitionId>,
}

impl ExtensionDefinition {
    pub fn new(
        file_id: SourceFileId,
        module_id: ModuleDefinitionId,
        node: &Arc<ast::Impl>,
    ) -> ExtensionDefinition {
        ExtensionDefinition {
            id: None,
            file_id,
            module_id,
            ast: node.clone(),
            pos: node.pos,
            type_params: None,
            ty: SourceType::Error,
            methods: Vec::new(),
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
        }
    }

    pub fn id(&self) -> ExtensionDefinitionId {
        self.id.expect("id missing")
    }

    pub fn type_params(&self) -> &TypeParamDefinition {
        self.type_params.as_ref().expect("uninitialized")
    }
}

impl Index<ExtensionDefinitionId> for Vec<RwLock<ExtensionDefinition>> {
    type Output = RwLock<ExtensionDefinition>;

    fn index(&self, index: ExtensionDefinitionId) -> &RwLock<ExtensionDefinition> {
        &self[index.to_usize()]
    }
}

mod matching {
    use crate::language::sem_analysis::{
        implements_trait, ExtensionDefinitionId, SemAnalysis, TypeParamDefinition,
    };
    use crate::language::ty::{SourceType, SourceTypeArray};

    pub fn extension_matches(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        extension_id: ExtensionDefinitionId,
    ) -> Option<SourceTypeArray> {
        let extension = sa.extensions[extension_id].read();
        extension_matches_ty(
            sa,
            check_ty,
            check_type_param_defs,
            extension.ty.clone(),
            extension.type_params(),
        )
    }

    pub fn extension_matches_ty(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
    ) -> Option<SourceTypeArray> {
        let mut bindings = vec![None; ext_type_param_defs.len()];

        let result = matches(
            sa,
            check_ty,
            check_type_param_defs,
            ext_ty.clone(),
            ext_type_param_defs,
            &mut bindings,
        );

        if result {
            Some(SourceTypeArray::with(
                bindings.into_iter().map(|t| t.unwrap()).collect(),
            ))
        } else {
            None
        }
    }

    fn matches(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
        bindings: &mut [Option<SourceType>],
    ) -> bool {
        if let SourceType::TypeParam(tp_id) = ext_ty {
            let binding = bindings[tp_id.to_usize()].clone();

            if let Some(binding) = binding {
                compare_concrete_types(
                    sa,
                    check_ty,
                    check_type_param_defs,
                    binding,
                    ext_type_param_defs,
                    bindings,
                )
            } else {
                let result = if check_ty.is_type_param() {
                    compare_type_param_bounds(
                        sa,
                        check_ty.clone(),
                        check_type_param_defs,
                        ext_ty,
                        ext_type_param_defs,
                    )
                } else {
                    concrete_type_fulfills_bounds(
                        sa,
                        check_ty.clone(),
                        check_type_param_defs,
                        ext_ty,
                        ext_type_param_defs,
                    )
                };

                bindings[tp_id.to_usize()] = Some(check_ty);

                result
            }
        } else {
            if check_ty.is_type_param() {
                false
            } else {
                compare_concrete_types(
                    sa,
                    check_ty,
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                    bindings,
                )
            }
        }
    }

    fn compare_type_param_bounds(
        _sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
    ) -> bool {
        let ext_tp_id = ext_ty.type_param_id().expect("expected type param");

        let check_tp_id = check_ty.type_param_id().expect("expected type param");

        for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
            if !check_type_param_defs.implements_trait(check_tp_id, trait_ty) {
                return false;
            }
        }

        true
    }

    fn concrete_type_fulfills_bounds(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
    ) -> bool {
        let ext_tp_id = ext_ty.type_param_id().expect("expected type param");

        for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
            if !implements_trait(sa, check_ty.clone(), check_type_param_defs, trait_ty) {
                return false;
            }
        }

        true
    }

    fn compare_concrete_types(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
        bindings: &mut [Option<SourceType>],
    ) -> bool {
        match check_ty {
            SourceType::Unit
            | SourceType::Bool
            | SourceType::Char
            | SourceType::UInt8
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::TypeParam(_) => check_ty == ext_ty,

            SourceType::Lambda(_, _) | SourceType::Trait(_, _) => {
                unimplemented!()
            }

            SourceType::Tuple(check_subtypes) => {
                if !ext_ty.is_tuple() {
                    return false;
                }

                let ext_subtypes = ext_ty.tuple_subtypes();

                if check_subtypes.len() != ext_subtypes.len() {
                    return false;
                }

                for (check_subty, ext_subty) in check_subtypes.iter().zip(ext_subtypes.iter()) {
                    if !matches(
                        sa,
                        check_subty.clone(),
                        check_type_param_defs,
                        ext_subty.clone(),
                        ext_type_param_defs,
                        bindings,
                    ) {
                        return false;
                    }
                }

                true
            }

            SourceType::Struct(check_struct_id, _) => {
                let ext_struct_id = if let Some(struct_id) = ext_ty.struct_id() {
                    struct_id
                } else {
                    return false;
                };

                if check_struct_id != ext_struct_id {
                    return false;
                }

                compare_type_params(
                    sa,
                    check_ty,
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                    bindings,
                )
            }

            SourceType::Enum(check_enum_id, _) => {
                let ext_enum_id = if let Some(enum_id) = ext_ty.enum_id() {
                    enum_id
                } else {
                    return false;
                };

                if check_enum_id != ext_enum_id {
                    return false;
                }

                compare_type_params(
                    sa,
                    check_ty,
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                    bindings,
                )
            }

            SourceType::Class(check_cls_id, _) => {
                let ext_cls_id = if let Some(cls_id) = ext_ty.cls_id() {
                    cls_id
                } else {
                    return false;
                };

                if check_cls_id != ext_cls_id {
                    return false;
                }

                compare_type_params(
                    sa,
                    check_ty,
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                    bindings,
                )
            }

            SourceType::Ptr | SourceType::Error | SourceType::This | SourceType::Any => {
                unreachable!()
            }
        }
    }

    fn compare_type_params(
        sa: &SemAnalysis,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
        bindings: &mut [Option<SourceType>],
    ) -> bool {
        let check_tps = check_ty.type_params();
        let ext_tps = ext_ty.type_params();

        assert_eq!(check_tps.len(), ext_tps.len());

        for (check_tp, ext_tp) in check_tps.iter().zip(ext_tps.iter()) {
            if !matches(
                sa,
                check_tp,
                check_type_param_defs,
                ext_tp,
                ext_type_param_defs,
                bindings,
            ) {
                return false;
            }
        }

        true
    }
}
