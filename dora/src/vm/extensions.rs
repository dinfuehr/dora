use parking_lot::RwLock;

use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use crate::ty::{implements_trait, SourceType};
use crate::vm::{FctId, FileId, NamespaceId, TypeParam, TypeParamId, VM};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExtensionId(u32);

impl From<usize> for ExtensionId {
    fn from(data: usize) -> ExtensionId {
        ExtensionId(data as u32)
    }
}

impl ExtensionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct ExtensionData {
    pub id: ExtensionId,
    pub file_id: FileId,
    pub ast: Arc<ast::Impl>,
    pub namespace_id: NamespaceId,
    pub pos: Position,
    pub type_params: Vec<TypeParam>,
    pub ty: SourceType,
    pub methods: Vec<FctId>,
    pub instance_names: HashMap<Name, FctId>,
    pub static_names: HashMap<Name, FctId>,
}

impl ExtensionData {
    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }
}

impl Index<ExtensionId> for Vec<RwLock<ExtensionData>> {
    type Output = RwLock<ExtensionData>;

    fn index(&self, index: ExtensionId) -> &RwLock<ExtensionData> {
        &self[index.to_usize()]
    }
}

pub fn extension_matches(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    extension_id: ExtensionId,
) -> bool {
    let extension = vm.extensions[extension_id].read();
    extension_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        extension.ty.clone(),
        &extension.type_params,
    )
}

pub fn extension_matches_ty(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    ext_ty: SourceType,
    ext_type_param_defs: &[TypeParam],
) -> bool {
    if ext_ty.is_type_param() {
        if check_ty.is_type_param() {
            compare_type_param_bounds(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
            )
        } else {
            concrete_type_fulfills_bounds(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
            )
        }
    } else {
        if check_ty.is_type_param() {
            false
        } else {
            compare_concrete_types(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
            )
        }
    }
}

fn compare_type_param_bounds(
    _vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    ext_ty: SourceType,
    ext_type_param_defs: &[TypeParam],
) -> bool {
    let ext_tp_id = ext_ty.type_param_id().expect("expected type param");
    let ext_tp_def = &ext_type_param_defs[ext_tp_id.to_usize()];

    let check_tp_id = check_ty.type_param_id().expect("expected type param");
    let check_tp_def = &check_type_param_defs[check_tp_id.to_usize()];

    for &trait_id in &ext_tp_def.trait_bounds {
        if !check_tp_def.trait_bounds.contains(&trait_id) {
            return false;
        }
    }

    true
}

fn concrete_type_fulfills_bounds(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    ext_ty: SourceType,
    ext_type_param_defs: &[TypeParam],
) -> bool {
    let ext_tp_id = ext_ty.type_param_id().expect("expected type param");
    let ext_tp_def = &ext_type_param_defs[ext_tp_id.to_usize()];

    for &trait_id in &ext_tp_def.trait_bounds {
        if !implements_trait(vm, check_ty.clone(), check_type_param_defs, trait_id) {
            return false;
        }
    }

    true
}

fn compare_concrete_types(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    ext_ty: SourceType,
    ext_type_param_defs: &[TypeParam],
) -> bool {
    match check_ty {
        SourceType::Unit
        | SourceType::Bool
        | SourceType::Char
        | SourceType::UInt8
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => check_ty == ext_ty,

        SourceType::Module(_) | SourceType::Lambda(_) | SourceType::TraitObject(_) => {
            unimplemented!()
        }

        SourceType::Tuple(check_tuple_id) => {
            let check_subtypes = vm.tuples.lock().get(check_tuple_id);

            let ext_tuple_id = if let Some(tuple_id) = ext_ty.tuple_id() {
                tuple_id
            } else {
                return false;
            };

            let ext_subtypes = vm.tuples.lock().get(ext_tuple_id);

            if check_subtypes.len() != ext_subtypes.len() {
                return false;
            }

            for (check_subty, ext_subty) in check_subtypes.iter().zip(ext_subtypes.iter()) {
                if !extension_matches_ty(
                    vm,
                    check_subty.clone(),
                    check_type_param_defs,
                    ext_subty.clone(),
                    ext_type_param_defs,
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
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
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
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
            )
        }

        SourceType::Class(check_cls_id, _) => {
            let ext_cls_id = if let Some(cls_id) = ext_ty.cls_id(vm) {
                cls_id
            } else {
                return false;
            };

            if check_cls_id != ext_cls_id {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
            )
        }

        SourceType::Ptr
        | SourceType::Error
        | SourceType::This
        | SourceType::Any
        | SourceType::TypeParam(_) => unreachable!(),
    }
}

fn compare_type_params(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    ext_ty: SourceType,
    ext_type_param_defs: &[TypeParam],
) -> bool {
    let check_tps = check_ty.type_params(vm);
    let ext_tps = ext_ty.type_params(vm);

    assert_eq!(check_tps.len(), ext_tps.len());

    for (check_tp, ext_tp) in check_tps.iter().zip(ext_tps.iter()) {
        if !extension_matches_ty(
            vm,
            check_tp,
            check_type_param_defs,
            ext_tp,
            ext_type_param_defs,
        ) {
            return false;
        }
    }

    true
}
