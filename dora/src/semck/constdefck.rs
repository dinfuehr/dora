use crate::semck;
use crate::ty::SourceType;
use crate::vm::{ConstId, FileId, NamespaceId, VM};

use dora_parser::ast;

pub fn check(vm: &VM) {
    for xconst in vm.consts.iter() {
        let (const_id, file_id, ast, namespace_id) = {
            let xconst = xconst.read();
            (
                xconst.id,
                xconst.file_id,
                xconst.ast.clone(),
                xconst.namespace_id,
            )
        };

        let mut clsck = ConstCheck {
            vm,
            const_id,
            file_id,
            ast: &ast,
            namespace_id,
        };

        clsck.check();
    }
}

struct ConstCheck<'x> {
    vm: &'x VM,
    const_id: ConstId,
    file_id: FileId,
    ast: &'x ast::Const,
    namespace_id: NamespaceId,
}

impl<'x> ConstCheck<'x> {
    fn check(&mut self) {
        let ty = semck::read_type_namespace(
            self.vm,
            self.file_id,
            self.namespace_id,
            &self.ast.data_type,
        )
        .unwrap_or(SourceType::Error);

        let xconst = self.vm.consts.idx(self.const_id);
        let mut xconst = xconst.write();
        xconst.ty = ty;
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            pos(1, 10),
            SemError::UnknownIdentifier("Foo".into()),
        );

        ok("const x: Int32 = 0;");
    }
}
