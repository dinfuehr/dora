use std::collections::HashMap;
use std::ops::Index;

use ast;
use ctxt::{Context, FctId};
use interner::Name;
use ty::BuiltinType;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ClassId(pub usize);

#[derive(Debug)]
pub struct Class<'ast> {
    pub id: ClassId,
    pub name: Name,
    pub ty: BuiltinType,
    pub parent_class: Option<ClassId>,
    pub derivable: bool,
    pub ctors: Vec<FctId>,
    pub fields: Vec<Field>,
    pub methods: Vec<FctId>,
    pub size: i32,
    pub ast: Option<&'ast ast::Class>,
}

impl<'ast> Class<'ast> {
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldId(pub usize);

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
