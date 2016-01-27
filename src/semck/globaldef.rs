use std::collections::HashMap;

use ast::*;
use ast::visit::*;

use ctxt::*;
use error::msg::Msg;
use lexer::position::Position;

use ty::BuiltinType;

pub fn check<'a, 'ast: 'a>(ctxt: &mut Context<'a, 'ast>) {
    let mut gdef = GlobalDef {
        ctxt: ctxt
    };

    gdef.visit_ast(ctxt.ast);
}

struct GlobalDef<'x, 'a: 'x, 'ast: 'a> {
    ctxt: &'x mut Context<'a, 'ast>
}

impl<'x, 'a, 'ast> Visitor<'ast> for GlobalDef<'x, 'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        let fct = FctContext {
            id: FctContextId(0),
            name: f.name,
            params_types: Vec::new(),
            return_type: BuiltinType::Unit,
            ast: Some(f),
            calls: HashMap::new(),
            defs: HashMap::new(),
            ir: None,
            tempsize: 0,
            localsize: 0,
            leaf: false,
            vars: Vec::new(),
            always_returns: false,
            code: FctCode::Uncompiled,
            stub: None,
        };

        if let Err(sym) = self.ctxt.add_function(fct) {
            let name = self.ctxt.interner.str(f.name).to_string();
            let msg = if sym.is_type() {
                Msg::ShadowType(name)
            } else {
                Msg::ShadowFunction(name)
            };

            report(self.ctxt, f.pos, msg);
        }
    }
}

fn report(ctxt: &Context, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}
