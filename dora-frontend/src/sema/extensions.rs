use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::sync::Arc;

use crate::interner::Name;
use crate::sema::{
    FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, SourceFileId, TypeParamDefinition,
};
use crate::ty::SourceType;
use crate::ParsedType;
use id_arena::Id;

pub use self::matching::{extension_matches, extension_matches_ty};
use dora_parser::ast;
use dora_parser::Span;

pub type ExtensionDefinitionId = Id<ExtensionDefinition>;

#[derive(Debug)]
pub struct ExtensionDefinition {
    pub id: OnceCell<ExtensionDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Impl>,
    pub span: Span,
    pub type_params: TypeParamDefinition,
    pub ty: OnceCell<Box<ParsedType>>,
    pub methods: OnceCell<Vec<FctDefinitionId>>,
    pub instance_names: RefCell<HashMap<Name, FctDefinitionId>>,
    pub static_names: RefCell<HashMap<Name, FctDefinitionId>>,
}

impl ExtensionDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Impl>,
        type_params: TypeParamDefinition,
    ) -> ExtensionDefinition {
        ExtensionDefinition {
            id: OnceCell::new(),
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
            type_params,
            ty: OnceCell::new(),
            methods: OnceCell::new(),
            instance_names: RefCell::new(HashMap::new()),
            static_names: RefCell::new(HashMap::new()),
        }
    }

    pub fn id(&self) -> ExtensionDefinitionId {
        self.id.get().cloned().expect("id missing")
    }

    pub fn type_param_definition(&self) -> &TypeParamDefinition {
        &self.type_params
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        self.ty.get().expect("missing type")
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn methods(&self) -> &[FctDefinitionId] {
        self.methods.get().expect("missing value")
    }
}

pub mod matching {
    use crate::sema::{
        implements_trait, ExtensionDefinitionId, Sema, TypeParamDefinition, TypeParamId,
    };
    use crate::ty::{SourceType, SourceTypeArray};

    pub fn extension_matches(
        sa: &Sema,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        extension_id: ExtensionDefinitionId,
    ) -> Option<SourceTypeArray> {
        let extension = sa.extension(extension_id);
        extension_matches_ty(
            sa,
            check_ty,
            check_type_param_defs,
            extension.ty().clone(),
            extension.type_param_definition(),
        )
    }

    pub fn extension_matches_ty(
        sa: &Sema,
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
        sa: &Sema,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
        bindings: &mut [Option<SourceType>],
    ) -> bool {
        if let SourceType::TypeParam(ext_tp_id) = ext_ty {
            let binding = bindings[ext_tp_id.to_usize()].clone();

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
                        ext_tp_id,
                        ext_type_param_defs,
                    )
                };

                bindings[ext_tp_id.to_usize()] = Some(check_ty);

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
        _sa: &Sema,
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
        sa: &Sema,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_tp_id: TypeParamId,
        ext_type_param_defs: &TypeParamDefinition,
    ) -> bool {
        for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
            if !implements_trait(sa, check_ty.clone(), check_type_param_defs, trait_ty) {
                return false;
            }
        }

        true
    }

    fn compare_concrete_types(
        sa: &Sema,
        check_ty: SourceType,
        check_type_param_defs: &TypeParamDefinition,
        ext_ty: SourceType,
        ext_type_param_defs: &TypeParamDefinition,
        bindings: &mut [Option<SourceType>],
    ) -> bool {
        if check_ty.is_error() || ext_ty.is_error() {
            return true;
        }

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

            SourceType::Lambda(check_params, check_ret_type) => match ext_ty {
                SourceType::Lambda(ext_params, ext_ret_type) => {
                    if check_params.len() != ext_params.len() {
                        return false;
                    }

                    for (ext_param, check_param) in ext_params.iter().zip(check_params.iter()) {
                        if !matches(
                            sa,
                            check_param.clone(),
                            check_type_param_defs,
                            ext_param.clone(),
                            ext_type_param_defs,
                            bindings,
                        ) {
                            return false;
                        }
                    }

                    matches(
                        sa,
                        *check_ret_type,
                        check_type_param_defs,
                        *ext_ret_type,
                        ext_type_param_defs,
                        bindings,
                    )
                }

                _ => false,
            },

            SourceType::Trait(check_trait_id, ..) => match ext_ty {
                SourceType::Trait(ext_trait_id, ..) => {
                    if check_trait_id != ext_trait_id {
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

                _ => false,
            },

            SourceType::TypeAlias(..) => {
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
        sa: &Sema,
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
