use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::convert::From;
use std::iter::Iterator;
use std::sync::Arc;

use crate::field::{Field, FieldId, FieldDef};
use crate::ty::BuiltinType;
use crate::typeparams::TypeParams;
use crate::utils::GrowableVec;
use crate::vm::VM;
use crate::vm::{FctId, FileId, ImplId, TraitId, TypeParam};
use crate::vtable::VTableBox;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ClassId(usize);

impl ClassId {
    pub fn max() -> ClassId {
        ClassId(usize::max_value())
    }
}

impl From<ClassId> for usize {
    fn from(data: ClassId) -> usize {
        data.0
    }
}

impl From<usize> for ClassId {
    fn from(data: usize) -> ClassId {
        ClassId(data)
    }
}

impl GrowableVec<RwLock<Class>> {
    pub fn idx(&self, index: ClassId) -> Arc<RwLock<Class>> {
        self.idx_usize(index.0)
    }
}

pub static DISPLAY_SIZE: usize = 6;

#[derive(Debug)]
pub struct Class {
    pub id: ClassId,
    pub file: FileId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ClassId>,
    pub has_open: bool,
    pub is_abstract: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub has_constructor: bool,

    pub constructor: Option<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub virtual_fcts: Vec<FctId>,

    pub traits: Vec<TraitId>,
    pub impls: Vec<ImplId>,

    pub type_params: Vec<TypeParam>,

    pub specializations: RwLock<HashMap<TypeParams, ClassDefId>>,

    // true if this class is the generic Array class
    pub is_array: bool,
    pub is_str: bool,
}

impl Class {
    pub fn is_generic(&self) -> bool {
        self.type_params.len() > 0
    }

    #[cfg(test)]
    pub fn field_by_name(&self, name: Name) -> FieldId {
        for field in &self.fields {
            if field.name == name {
                return field.id;
            }
        }

        panic!("field not found!")
    }

    pub fn long_name(&self, vm: &VM) -> String {
        let name = vm.interner.str(self.name);

        let params = if self.type_params.len() > 0 {
            self.type_params
                .iter()
                .map(|p| vm.interner.str(p.name).to_string())
                .collect::<Vec<_>>()
                .join(", ")
        } else {
            return name.to_string();
        };

        format!("{}<{}>", name, params)
    }

    pub fn find_impl_for_trait(&self, vm: &VM, trait_id: TraitId) -> Option<ImplId> {
        for &impl_id in &self.impls {
            let ximpl = vm.impls[impl_id].read();

            if ximpl.trait_id == Some(trait_id) {
                return Some(impl_id);
            }
        }

        None
    }

    pub fn find_field(&self, vm: &VM, name: Name) -> Option<(ClassId, FieldId)> {
        let mut classid = self.id;

        loop {
            let cls = vm.classes.idx(classid);
            let cls = cls.read();

            for field in &cls.fields {
                if field.name == name {
                    return Some((classid, field.id));
                }
            }

            if let Some(parent_class) = cls.parent_class {
                classid = parent_class;
            } else {
                return None;
            }
        }
    }

    pub fn find_method(&self, vm: &VM, name: Name, is_static: bool) -> Option<FctId> {
        let mut classid = self.id;

        loop {
            let cls = vm.classes.idx(classid);
            let cls = cls.read();

            for &method in &cls.methods {
                let method = vm.fcts.idx(method);
                let method = method.read();

                if method.name == name && method.is_static == is_static {
                    return Some(method.id);
                }
            }

            if let Some(parent_class) = cls.parent_class {
                classid = parent_class;
            } else {
                return None;
            }
        }
    }

    pub fn find_trait_method(
        &self,
        vm: &VM,
        trait_id: TraitId,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
        for &impl_id in &self.impls {
            let ximpl = vm.impls[impl_id].read();

            if ximpl.trait_id != Some(trait_id) {
                continue;
            }

            for &method in &ximpl.methods {
                let method = vm.fcts.idx(method);
                let method = method.read();

                if method.name == name && method.is_static == is_static {
                    return Some(method.id);
                }
            }
        }

        None
    }

    pub fn find_methods(&self, vm: &VM, name: Name, is_static: bool) -> Vec<FctId> {
        let mut classid = self.id;
        let mut candidates = Vec::new();
        let mut ignores = HashSet::new();

        loop {
            let cls = vm.classes.idx(classid);
            let cls = cls.read();

            for &method in &cls.methods {
                let method = vm.fcts.idx(method);
                let method = method.read();

                if method.name == name && method.is_static == is_static {
                    if let Some(overrides) = method.overrides {
                        ignores.insert(overrides);
                    }

                    if !ignores.contains(&method.id) {
                        return vec![method.id];
                    }
                }
            }

            if let Some(parent_class) = cls.parent_class {
                classid = parent_class;
            } else {
                break;
            }
        }

        classid = self.id;

        loop {
            let cls = vm.classes.idx(classid);
            let cls = cls.read();

            for &impl_id in &cls.impls {
                let ximpl = vm.impls[impl_id].read();

                for &method in &ximpl.methods {
                    let method = vm.fcts.idx(method);
                    let method = method.read();

                    if method.name == name && method.is_static == is_static {
                        candidates.push(method.id);
                    }
                }
            }

            if let Some(parent_class) = cls.parent_class {
                classid = parent_class;
            } else {
                break;
            }
        }

        candidates
    }

    pub fn subclass_from(&self, vm: &VM, super_id: ClassId) -> bool {
        let mut cls_id = self.id;

        loop {
            if cls_id == super_id {
                return true;
            }

            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            match cls.parent_class {
                Some(id) => {
                    cls_id = id;
                }

                None => {
                    return false;
                }
            }
        }
    }
}



#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ClassDefId(usize);

impl ClassDefId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ClassDefId {
    fn from(data: usize) -> ClassDefId {
        ClassDefId(data)
    }
}

impl GrowableVec<RwLock<ClassDef>> {
    pub fn idx(&self, index: ClassDefId) -> Arc<RwLock<ClassDef>> {
        self.idx_usize(index.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassSize {
    Fixed(i32),
    Array(i32),
    ObjArray,
    FreeArray,
    Str,
}

#[derive(Debug)]
pub struct ClassDef {
    pub id: ClassDefId,
    pub cls_id: Option<ClassId>,
    pub type_params: TypeParams,
    pub parent_id: Option<ClassDefId>,
    pub fields: Vec<FieldDef>,
    pub size: ClassSize,
    pub ref_fields: Vec<i32>,
    pub vtable: Option<VTableBox>,
}

impl ClassDef {
    pub fn name(&self, vm: &VM) -> String {
        if let Some(cls_id) = self.cls_id {
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            let name = vm.interner.str(cls.name);

            let params = if self.type_params.len() > 0 {
                self.type_params
                    .iter()
                    .map(|p| p.name(vm))
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                return name.to_string();
            };

            format!("{}<{}>", name, params)
        } else {
            "<Unknown>".into()
        }
    }
}
