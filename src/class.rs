use std::collections::HashSet;
use std::convert::From;
use std::ops::{Index, IndexMut};

use ast;
use ctxt::{Context, Fct, FctId};
use interner::Name;
use vtable::VTable;
use ty::BuiltinType;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ClassId(usize);

impl From<usize> for ClassId {
    fn from(data: usize) -> ClassId {
        ClassId(data)
    }
}

impl<'ast> Index<ClassId> for Vec<Box<Class<'ast>>> {
    type Output = Class<'ast>;

    fn index(&self, index: ClassId) -> &Class<'ast> {
        &self[index.0]
    }
}

impl<'ast> IndexMut<ClassId> for Vec<Box<Class<'ast>>> {
    fn index_mut(&mut self, index: ClassId) -> &mut Class<'ast> {
        &mut self[index.0]
    }
}

pub static DISPLAY_SIZE: usize = 6;

#[derive(Debug)]
pub struct Class<'ast> {
    pub id: ClassId,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ClassId>,
    pub has_open: bool,
    pub internal: bool,
    pub primary_ctor: bool,

    pub ctors: Vec<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub size: i32,
    pub ast: Option<&'ast ast::Class>,
    pub vtable: Option<Box<VTable<'ast>>>,
}

impl<'ast> Class<'ast> {
    pub fn all_fields<'a>(&'a self, ctxt: &'a Context<'ast>) -> FieldIterator<'a, 'ast> {
        FieldIterator {
            ctxt: ctxt,
            class: self,
            field_idx: 0,
        }
    }
    pub fn find_field(&self, ctxt: &Context, name: Name) -> Option<(ClassId, FieldId)> {
        let mut classid = self.id;

        loop {
            let cls = ctxt.cls_by_id(classid);

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

    pub fn find_method(&self, ctxt: &Context, name: Name,
                       args: &[BuiltinType]) -> Option<FctId> {
        let mut classid = self.id;

        loop {
            let cls = ctxt.cls_by_id(classid);

            for &method in &cls.methods {
                let method = ctxt.fct_by_id(method);

                if method.name == name && method.params_types == args {
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

    pub fn find_methods_with<F>(&self, ctxt: &Context, name: Name,
                                f: F) -> Vec<FctId> where F: Fn(&Fct) -> bool {
        let mut classid = self.id;
        let mut candidates = Vec::new();
        let mut ignores = HashSet::new();

        loop {
            let cls = ctxt.cls_by_id(classid);

            for &method in &cls.methods {
                let method = ctxt.fct_by_id(method);

                if method.name == name && f(method) {
                    if let Some(overrides) = method.overrides {
                        ignores.insert(overrides);
                    }

                    if !ignores.contains(&method.id) {
                        candidates.push(method.id);
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

    pub fn subclass_from(&self, ctxt: &Context<'ast>, super_id: ClassId) -> bool {
        let mut class = self;

        loop {
            if class.id == super_id {
                return true;
            }

            match class.parent_class {
                Some(id) => {
                    class = ctxt.cls_by_id(id);
                }

                None => { return false; }
            }
        }
    }
}

pub struct FieldIterator<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    class: &'a Class<'ast>,
    field_idx: usize
}

impl<'a, 'ast> Iterator for FieldIterator<'a, 'ast> {
    type Item = &'a Field;

    fn next(&mut self) -> Option<&'a Field> {
        if self.field_idx < self.class.fields.len() {
            let idx = self.field_idx;
            self.field_idx = idx + 1;
            return Some(&self.class.fields[idx]);

        } else if let Some(parent_class) = self.class.parent_class {
            self.class = self.ctxt.cls_by_id(parent_class);
            let number_fields = self.class.fields.len();

            if number_fields > 0 {
                self.field_idx = 1;
                return Some(&self.class.fields[0]);

            }
        }

        None
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
