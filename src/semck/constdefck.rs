use ctxt::{ConstId, NodeMap, SemContext};
use dora_parser::ast;
use dora_parser::ast::visit::Visitor;
use dora_parser::error::msg::Msg;
use dora_parser::lexer::position::Position;
use semck;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut SemContext<'ast>, map_const_defs: &NodeMap<ConstId>) {
    let mut clsck = ConstCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        const_id: None,
        map_const_defs: map_const_defs,
    };

    clsck.check();
}

struct ConstCheck<'x, 'ast: 'x> {
    ctxt: &'x mut SemContext<'ast>,
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

        let mut xconst = self.ctxt.consts[const_id].borrow_mut();
        xconst.ty = semck::read_type(self.ctxt, &c.data_type).unwrap_or(BuiltinType::Unit);
    }
}

fn report(ctxt: &SemContext, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            pos(1, 10),
            Msg::UnknownType("Foo".into()),
        );

        ok("const x: int = 0;");
    }
}
