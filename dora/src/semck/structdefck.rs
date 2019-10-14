use crate::error::msg::SemError;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{NodeMap, StructFieldData, StructId, VM};

use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_struct_defs: &NodeMap<StructId>) {
    let mut clsck = StructCheck {
        vm,
        ast,
        struct_id: None,
        map_struct_defs,
    };

    clsck.check();
}

struct StructCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_struct_defs: &'x NodeMap<StructId>,

    struct_id: Option<StructId>,
}

impl<'x, 'ast> StructCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for StructCheck<'x, 'ast> {
    fn visit_struct(&mut self, s: &'ast ast::Struct) {
        self.struct_id = Some(*self.map_struct_defs.get(s.id).unwrap());

        visit::walk_struct(self, s);

        self.struct_id = None;
    }

    fn visit_struct_field(&mut self, f: &'ast ast::StructField) {
        let id = self.struct_id.unwrap();
        let struc = self.vm.structs.idx(id);
        let file = struc.lock().file;

        let ty = semck::read_type(self.vm, file, &f.data_type).unwrap_or(BuiltinType::Unit);

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
        ok("struct Foo { a: Int }");
        ok("struct Foo { a: Int, b: Int }");
        ok("struct Foo { a: Int } struct Bar { a: Int }");
        ok("struct Foo { a: Int, bar: Bar } struct Bar { a: Int }");
        err(
            "struct Bar { a: Unknown }",
            pos(1, 17),
            SemError::UnknownType("Unknown".into()),
        );
        err(
            "struct Foo { a: Int, a: Int }",
            pos(1, 22),
            SemError::ShadowField("a".into()),
        );
    }
}
