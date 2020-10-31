use crate::semck;
use crate::ty::SourceType;
use crate::vm::{ConstId, NodeMap, VM};

use dora_parser::ast;
use dora_parser::ast::visit::Visitor;

pub fn check(vm: &mut VM, map_const_defs: &NodeMap<ConstId>) {
    let mut clsck = ConstCheck {
        vm,
        const_id: None,
        map_const_defs,
    };

    clsck.check();
}

struct ConstCheck<'x> {
    vm: &'x mut VM,
    map_const_defs: &'x NodeMap<ConstId>,

    const_id: Option<ConstId>,
}

impl<'x> ConstCheck<'x> {
    fn check(&mut self) {
        let files = self.vm.files.clone();
        let files = files.read();

        for file in files.iter() {
            self.visit_file(file);
        }
    }
}

impl<'x> Visitor for ConstCheck<'x> {
    fn visit_const(&mut self, c: &ast::Const) {
        let const_id = *self.map_const_defs.get(c.id).unwrap();

        let xconst = self.vm.consts.idx(const_id);
        let mut xconst = xconst.lock();
        xconst.ty =
            semck::read_type_namespace(self.vm, xconst.file, xconst.namespace_id, &c.data_type)
                .unwrap_or(SourceType::Unit);
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
            SemError::UnknownType("Foo".into()),
        );

        ok("const x: Int32 = 0;");
    }
}
