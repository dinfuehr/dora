use parking_lot::RwLock;

use std::collections::{HashMap, HashSet};
use std::convert::From;
use std::iter::Iterator;
use std::sync::Arc;

use crate::semck::specialize::replace_type_param;
use crate::size::InstanceSize;
use crate::sym::SymLevel;
use crate::ty::{BuiltinType, TypeList, TypeListId};
use crate::utils::GrowableVec;
use crate::vm::VM;
use crate::vm::{ExtensionId, FctId, Field, FieldDef, FieldId, FileId, ImplId, TraitId};
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
    pub parent_class: Option<BuiltinType>,
    pub has_open: bool,
    pub is_abstract: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub has_constructor: bool,
    pub table: SymLevel,

    pub constructor: Option<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub virtual_fcts: Vec<FctId>,

    pub traits: Vec<TraitId>,
    pub impls: Vec<ImplId>,
    pub extensions: Vec<ExtensionId>,

    pub type_params: Vec<TypeParam>,

    pub specializations: RwLock<HashMap<TypeList, ClassDefId>>,

    // true if this class is the generic Array class
    pub is_array: bool,
    pub is_str: bool,
}

impl Class {
    pub fn is_generic(&self) -> bool {
        self.type_params.len() > 0
    }

    pub fn type_param(&self, id: TypeListId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn type_param_ty(&self, ty: BuiltinType) -> &TypeParam {
        let id = match ty {
            BuiltinType::TypeParam(id) => id,
            _ => unimplemented!(),
        };

        &self.type_params[id.to_usize()]
    }

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

        if self.type_params.len() > 0 {
            let type_params = self
                .type_params
                .iter()
                .map(|p| vm.interner.str(p.name).to_string())
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_params)
        } else {
            name.to_string()
        }
    }

    pub fn name_with_params(&self, vm: &VM, type_list: &TypeList) -> String {
        let name = vm.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name(vm))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
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
                classid = parent_class.cls_id(vm).expect("no class");
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

    pub fn subclass_from(&self, vm: &VM, super_id: ClassId) -> bool {
        let mut cls_id = self.id;

        loop {
            if cls_id == super_id {
                return true;
            }

            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            match cls.parent_class {
                Some(parent_class) => {
                    cls_id = parent_class.cls_id(vm).expect("no class");
                }

                None => {
                    return false;
                }
            }
        }
    }

    pub fn implements_trait(&self, vm: &VM, trait_id: TraitId) -> bool {
        self.traits.contains(&trait_id) || vm.known.traits.zero == trait_id && !self.ty.is_cls()
    }
}

pub fn find_field_in_class(
    vm: &VM,
    mut class: BuiltinType,
    name: Name,
) -> Option<(BuiltinType, FieldId, BuiltinType)> {
    loop {
        let cls_id = class.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        let type_list = class.type_params(vm);

        for field in &cls.fields {
            if field.name == name {
                return Some((
                    class,
                    field.id,
                    replace_type_param(vm, field.ty, &type_list, None),
                ));
            }
        }

        if let Some(parent_class) = cls.parent_class {
            let type_list = parent_class.type_params(vm);
            class = replace_type_param(vm, parent_class, &type_list, None);
        } else {
            return None;
        }
    }
}

pub fn find_method_in_class(
    vm: &VM,
    mut class: BuiltinType,
    name: Name,
) -> Option<(BuiltinType, FctId)> {
    loop {
        let cls_id = class.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &method in &cls.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name && method.is_static == false {
                return Some((class, method.id));
            }
        }

        if let Some(parent_class) = cls.parent_class {
            let type_list = parent_class.type_params(vm);
            class = replace_type_param(vm, parent_class, &type_list, None);
        } else {
            return None;
        }
    }
}

pub fn find_methods_in_class(
    vm: &VM,
    object_type: BuiltinType,
    name: Name,
    is_static: bool,
) -> Vec<(BuiltinType, FctId)> {
    let mut candidates = Vec::new();
    let mut ignores = HashSet::new();

    let mut class_type = object_type;

    loop {
        let cls_id = class_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &method in &cls.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name && method.is_static == is_static {
                if let Some(overrides) = method.overrides {
                    ignores.insert(overrides);
                }

                if !ignores.contains(&method.id) {
                    return vec![(class_type, method.id)];
                }
            }
        }

        if let Some(parent_class) = cls.parent_class {
            let type_list = class_type.type_params(vm);
            class_type = replace_type_param(vm, parent_class, &type_list, None);
        } else {
            break;
        }
    }

    // Find extension methods
    {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &extension_id in &cls.extensions {
            let extension = vm.extensions[extension_id].read();

            if extension.ty.type_params(vm) != object_type.type_params(vm) {
                continue;
            }

            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.get(&name) {
                return vec![(extension.ty, fct_id)];
            }
        }
    }

    let mut class_type = object_type;

    loop {
        let cls_id = class_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();

        for &impl_id in &cls.impls {
            let ximpl = vm.impls[impl_id].read();

            if ximpl.class_ty.type_params(vm) != class_type.type_params(vm) {
                continue;
            }

            for &method in &ximpl.methods {
                let method = vm.fcts.idx(method);
                let method = method.read();

                if method.name == name && method.is_static == is_static {
                    candidates.push((class_type, method.id));
                }
            }
        }

        if let Some(parent_class) = cls.parent_class {
            let type_list = class_type.type_params(vm);
            class_type = replace_type_param(vm, parent_class, &type_list, None);
        } else {
            break;
        }
    }

    candidates
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub name: Name,
    pub trait_bounds: HashSet<TraitId>,
}

impl TypeParam {
    pub fn new(name: Name) -> TypeParam {
        TypeParam {
            name,
            trait_bounds: HashSet::new(),
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

#[derive(Debug)]
pub struct ClassDef {
    pub id: ClassDefId,
    pub cls_id: Option<ClassId>,
    pub type_params: TypeList,
    pub parent_id: Option<ClassDefId>,
    pub fields: Vec<FieldDef>,
    pub size: InstanceSize,
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
