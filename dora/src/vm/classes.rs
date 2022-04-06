use parking_lot::RwLock;

use std::convert::From;
use std::iter::Iterator;
use std::ops::{Index, IndexMut};
use std::sync::Arc;

use crate::language::sem_analysis::ClassDefinitionId;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::size::InstanceSize;
use crate::utils::GrowableVec;
use crate::vm::VM;
use crate::vtable::VTableBox;
use dora_parser::interner::Name;

pub static DISPLAY_SIZE: usize = 6;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ClassInstanceId(usize);

impl ClassInstanceId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ClassInstanceId {
    fn from(data: usize) -> ClassInstanceId {
        ClassInstanceId(data)
    }
}

impl GrowableVec<ClassInstance> {
    pub fn idx(&self, index: ClassInstanceId) -> Arc<ClassInstance> {
        self.idx_usize(index.0)
    }
}

#[derive(Debug)]
pub struct ClassInstance {
    pub id: ClassInstanceId,
    pub cls_id: Option<ClassDefinitionId>,
    pub trait_object: Option<SourceType>,
    pub type_params: SourceTypeArray,
    pub parent_id: Option<ClassInstanceId>,
    pub fields: Vec<FieldDef>,
    pub size: InstanceSize,
    pub ref_fields: Vec<i32>,
    pub vtable: RwLock<Option<VTableBox>>,
}

impl ClassInstance {
    pub fn name(&self, vm: &VM) -> String {
        if let Some(cls_id) = self.cls_id {
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            let name = vm.interner.str(cls.name);

            let params = if self.type_params.len() > 0 {
                self.type_params
                    .iter()
                    .map(|p| p.name_cls(vm, &*cls))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                return name.to_string();
            };

            format!("{}[{}]", name, params)
        } else {
            "<Unknown>".into()
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(usize);

impl FieldId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for FieldId {
    fn from(data: usize) -> FieldId {
        FieldId(data)
    }
}

#[derive(Debug)]
pub struct Field {
    pub id: FieldId,
    pub name: Name,
    pub ty: SourceType,
    pub offset: i32,
    pub mutable: bool,
    pub is_pub: bool,
}

impl Index<FieldId> for Vec<Field> {
    type Output = Field;

    fn index(&self, index: FieldId) -> &Field {
        &self[index.0]
    }
}

impl IndexMut<FieldId> for Vec<Field> {
    fn index_mut(&mut self, index: FieldId) -> &mut Field {
        &mut self[index.0]
    }
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub offset: i32,
    pub ty: SourceType,
}
