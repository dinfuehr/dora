use parking_lot::RwLock;
use std::collections::hash_map::HashMap;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{accessible_from, FileId, NamespaceId, TypeList, TypeParam, VM};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(u32);

impl GrowableVec<RwLock<StructData>> {
    pub fn idx(&self, index: StructId) -> Arc<RwLock<StructData>> {
        self.idx_usize(index.0 as usize)
    }
}

impl From<u32> for StructId {
    fn from(data: u32) -> StructId {
        StructId(data)
    }
}

#[derive(Debug)]
pub struct StructData {
    pub id: StructId,
    pub file_id: FileId,
    pub ast: Arc<ast::Struct>,
    pub namespace_id: NamespaceId,
    pub type_params: Vec<TypeParam>,
    pub is_pub: bool,
    pub pos: Position,
    pub name: Name,
    pub fields: Vec<StructFieldData>,
    pub specializations: RwLock<HashMap<TypeList, StructDefId>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StructFieldId(u32);

impl From<u32> for StructFieldId {
    fn from(data: u32) -> StructFieldId {
        StructFieldId(data)
    }
}

#[derive(Debug)]
pub struct StructFieldData {
    pub id: StructFieldId,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructDefId(usize);

impl From<usize> for StructDefId {
    fn from(data: usize) -> StructDefId {
        StructDefId(data)
    }
}

impl GrowableVec<RwLock<StructDef>> {
    pub fn idx(&self, index: StructDefId) -> Arc<RwLock<StructDef>> {
        self.idx_usize(index.0)
    }
}

pub struct StructDef {
    pub fields: Vec<StructFieldDef>,
    pub size: i32,
    pub align: i32,
    pub ref_fields: Vec<i32>,
}

#[derive(Debug, Clone)]
pub struct StructFieldDef {
    pub offset: i32,
    pub ty: SourceType,
}

pub fn struct_accessible_from(vm: &VM, struct_id: StructId, namespace_id: NamespaceId) -> bool {
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    accessible_from(vm, xstruct.namespace_id, xstruct.is_pub, namespace_id)
}
