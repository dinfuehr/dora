use crate::error::msg::SemError;
use crate::semck;
use crate::ty::SourceType;
use crate::vm::{FileId, NamespaceId, StructFieldData, StructId, VM};

use dora_parser::ast;

pub fn check(vm: &VM) {
    for xstruct in vm.structs.iter() {
        let (struct_id, file_id, ast, namespace_id) = {
            let xstruct = xstruct.read();
            (
                xstruct.id,
                xstruct.file_id,
                xstruct.ast.clone(),
                xstruct.namespace_id,
            )
        };

        let mut clsck = StructCheck {
            vm,
            struct_id,
            file_id,
            ast: &ast,
            namespace_id,
        };

        clsck.check();
    }
}

struct StructCheck<'x> {
    vm: &'x VM,
    struct_id: StructId,
    file_id: FileId,
    ast: &'x ast::Struct,
    namespace_id: Option<NamespaceId>,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        for field in &self.ast.fields {
            self.visit_struct_field(field);
        }
    }

    fn visit_struct_field(&mut self, f: &ast::StructField) {
        let ty = semck::read_type_namespace(self.vm, self.file_id, self.namespace_id, &f.data_type)
            .unwrap_or(SourceType::Error);

        let xstruct = self.vm.structs.idx(self.struct_id);
        let mut xstruct = xstruct.write();

        for field in &xstruct.fields {
            if field.name == f.name {
                let name = self.vm.interner.str(f.name).to_string();
                self.vm
                    .diag
                    .lock()
                    .report(self.file_id, f.pos, SemError::ShadowField(name));
                return;
            }
        }

        let field = StructFieldData {
            id: (xstruct.fields.len() as u32).into(),
            pos: f.pos,
            name: f.name,
            ty,
        };

        xstruct.fields.push(field);
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
