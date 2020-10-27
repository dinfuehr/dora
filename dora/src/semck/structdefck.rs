use crate::error::msg::SemError;
use crate::semck;
use crate::ty::SourceType;
use crate::vm::{NodeMap, StructFieldData, StructId, VM};

use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};

pub fn check(vm: &mut VM, map_struct_defs: &NodeMap<StructId>) {
    let mut clsck = StructCheck {
        vm,
        struct_id: None,
        map_struct_defs,
    };

    clsck.check();
}

struct StructCheck<'x> {
    vm: &'x mut VM,
    map_struct_defs: &'x NodeMap<StructId>,

    struct_id: Option<StructId>,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        let files = self.vm.files.clone();
        let files = files.read();

        for file in files.iter() {
            self.visit_file(file);
        }
    }
}

impl<'x> Visitor for StructCheck<'x> {
    fn visit_struct(&mut self, s: &ast::Struct) {
        self.struct_id = Some(*self.map_struct_defs.get(s.id).unwrap());

        visit::walk_struct(self, s);

        self.struct_id = None;
    }

    fn visit_struct_field(&mut self, f: &ast::StructField) {
        let id = self.struct_id.unwrap();
        let struc = self.vm.structs.idx(id);
        let file = struc.lock().file;

        let ty = semck::read_type(self.vm, file, &f.data_type).unwrap_or(SourceType::Unit);

        let mut struc = struc.lock();

        for field in &struc.fields {
            if field.name == f.name {
                let name = self.vm.interner.str(f.name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report(struc.file, f.pos, SemError::ShadowField(name));
                return;
            }
        }

        let field = StructFieldData {
            id: (struc.fields.len() as u32).into(),
            pos: f.pos,
            name: f.name,
            ty,
        };

        struc.fields.push(field);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn struct_field() {
        ok("struct Foo { a: Int32 }");
        ok("struct Foo { a: Int32, b: Int32 }");
        ok("struct Foo { a: Int32 } struct Bar { a: Int32 }");
        ok("struct Foo { a: Int32, bar: Bar } struct Bar { a: Int32 }");
        err(
            "struct Bar { a: Unknown }",
            pos(1, 17),
            SemError::UnknownType("Unknown".into()),
        );
        err(
            "struct Foo { a: Int32, a: Int32 }",
            pos(1, 24),
            SemError::ShadowField("a".into()),
        );
    }
}
