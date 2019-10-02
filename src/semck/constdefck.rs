use crate::error::msg::Msg;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{ConstId, NodeMap, VM};

use dora_parser::ast::visit::Visitor;
use dora_parser::ast::{self, Ast};
use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_const_defs: &NodeMap<ConstId>) {
    let mut clsck = ConstCheck {
        vm: vm,
        ast: ast,
        const_id: None,
        map_const_defs: map_const_defs,
    };

    clsck.check();
}

struct ConstCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_const_defs: &'x NodeMap<ConstId>,

    const_id: Option<ConstId>,
}

impl<'x, 'ast> ConstCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for ConstCheck<'x, 'ast> {
    fn visit_const(&mut self, c: &'ast ast::Const) {
        let const_id = *self.map_const_defs.get(c.id).unwrap();

        let xconst = self.vm.consts.idx(const_id);
        let mut xconst = xconst.lock();
        xconst.ty = semck::read_type(self.vm, &c.data_type).unwrap_or(BuiltinType::Unit);
    }
}

fn report(vm: &VM, pos: Position, msg: Msg) {
    vm.diag.lock().report_without_path(pos, msg);
}

#[cfg(test)]
mod tests {
    use crate::error::msg::Msg;
    use crate::semck::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            pos(1, 10),
            Msg::UnknownType("Foo".into()),
        );

        ok("const x: Int = 0;");
    }
}
