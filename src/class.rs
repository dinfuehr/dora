use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::From;
use std::ops::{Index, IndexMut};

use ctxt::{Context, Fct, FctId, ImplId, TraitId};
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;
use vtable::VTableBox;
use ty::BuiltinType;
use utils::GrowableVec;

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

impl Index<ClassId> for GrowableVec<Class> {
    type Output = RefCell<Class>;

    fn index(&self, index: ClassId) -> &RefCell<Class> {
        &self[index.0]
    }
}

pub static DISPLAY_SIZE: usize = 6;

#[derive(Debug)]
pub struct Class {
    pub id: ClassId,
    pub pos: Position,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ClassId>,
    pub has_open: bool,
    pub internal: bool,
    pub internal_resolved: bool,
    pub primary_ctor: bool,

    pub ctors: Vec<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub size: i32,
    pub vtable: Option<VTableBox>,

    pub traits: Vec<TraitId>,
    pub impls: Vec<ImplId>,

    pub type_params: Vec<Name>,
    pub is_generic: bool,
    pub specialization_for: Option<ClassId>,
    pub specialization_params: Vec<BuiltinType>,
    pub specializations: RefCell<HashMap<Vec<BuiltinType>, ClassId>>,

    // true if this class is specialization of generic Array class
    pub is_array: bool,

    // true if `is_array` is true and element type is some reference type
    pub is_object_array: bool,

    // size of single element in array
    pub element_size: i32,

    /// contains offset of all reference fields in this class.
    /// In contrast to `fields` it also stores fields of super classes.
    pub ref_fields: Vec<i32>,
}

impl Class {
    pub fn is_generic(&self) -> bool {
        self.is_generic
    }

    pub fn long_name(&self, ctxt: &Context) -> String {
        let name = ctxt.interner.str(self.name);

        let params = if self.type_params.len() > 0 {
            self.type_params
                .iter()
                .map(|&n| ctxt.interner.str(n).to_string())
                .collect::<Vec<_>>()
                .join(", ")

        } else if self.specialization_params.len() > 0 {
            self.specialization_params
                .iter()
                .map(|&ty| ty.name(ctxt))
                .collect::<Vec<_>>()
                .join(", ")

        } else {
            return name.to_string();
        };

        format!("{}<{}>", name, params)
    }

    pub fn find_field(&self, ctxt: &Context, name: Name) -> Option<(ClassId, FieldId)> {
        let mut classid = self.id;

        loop {
            let cls = ctxt.classes[classid].borrow();

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

    pub fn find_method(&self, ctxt: &Context, name: Name, is_static: bool) -> Option<FctId> {
        let mut classid = self.id;

        loop {
            let cls = ctxt.classes[classid].borrow();

            for &method in &cls.methods {
                let method = ctxt.fcts[method].borrow();

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

    pub fn find_methods_with<F>(&self, ctxt: &Context, name: Name, is_static: bool, f: F) -> Vec<FctId>
        where F: Fn(&Fct) -> bool
    {
        let mut classid = self.id;
        let mut candidates = Vec::new();
        let mut ignores = HashSet::new();

        loop {
            let cls = ctxt.classes[classid].borrow();

            for &method in &cls.methods {
                let method = ctxt.fcts[method].borrow();

                if method.name == name && method.is_static == is_static && f(&*method) {
                    if let Some(overrides) = method.overrides {
                        ignores.insert(overrides);
                    }

                    if !ignores.contains(&method.id) {
                        candidates.push(method.id);
                    }
                }
            }

            for &impl_id in &cls.impls {
                let ximpl = ctxt.impls[impl_id].borrow();

                for &method in &ximpl.methods {
                    let method = ctxt.fcts[method].borrow();

                    if method.name == name && f(&*method) {
                        if let Some(overrides) = method.overrides {
                            ignores.insert(overrides);
                        }

                        if !ignores.contains(&method.id) {
                            candidates.push(method.id);
                        }
                    }
                }
            }

            if let Some(parent_class) = cls.parent_class {
                classid = parent_class;

            } else {
                return candidates;
            }
        }
    }

    pub fn subclass_from(&self, ctxt: &Context, super_id: ClassId) -> bool {
        let mut cls_id = self.id;

        loop {
            if cls_id == super_id {
                return true;
            }

            let cls = ctxt.classes[cls_id].borrow();

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

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamId(usize);

impl TypeParamId {
    pub fn idx(self) -> usize {
        self.0
    }
}

impl From<usize> for TypeParamId {
    fn from(data: usize) -> TypeParamId {
        TypeParamId(data)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(usize);

impl From<usize> for FieldId {
    fn from(data: usize) -> FieldId {
        FieldId(data)
    }
}

#[derive(Debug)]
pub struct Field {
    pub id: FieldId,
    pub name: Name,
    pub ty: BuiltinType,
    pub offset: i32,
    pub reassignable: bool,
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
