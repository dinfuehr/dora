use std::sync::{Arc, Mutex};

use ast::*;
use ast::visit::*;
use class::{self, ClassId};
use ctxt::*;
use error::msg::Msg;
use interner::Name;
use lexer::position::Position;
use sym::Sym::{self, SymClass};
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    let mut gdef = GlobalDef {
        ctxt: ctxt
    };

    gdef.visit_ast(ctxt.ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    ctxt: &'x mut Context<'ast>
}

impl<'x, 'ast> Visitor<'ast> for GlobalDef<'x, 'ast> {
    fn visit_class(&mut self, c: &'ast Class) {
        let id: ClassId = self.ctxt.classes.len().into();
        let cls = class::Class {
            id: id,
            name: c.name,
            ty: BuiltinType::Class(id),
            parent_class: None,
            has_open: c.has_open,
            internal: c.internal,
            ctors: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            ast: Some(c),
            size: 0,
            vtable: None,
        };

        self.ctxt.classes.push(Box::new(cls));
        let sym = SymClass(id);

        assert!(self.ctxt.cls_defs.insert(c.id, id).is_none());

        if let Some(sym) = self.ctxt.sym.borrow_mut().insert(c.name, sym) {
            report(self.ctxt, c.name, c.pos, sym);
        }
    }

    fn visit_fct(&mut self, f: &'ast Function) {
        let kind = if f.block.is_some() {
            FctKind::Source(Arc::new(Mutex::new(FctSrc::new(f))))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            pos: f.pos,
            name: f.name,
            params_types: Vec::new(),
            return_type: BuiltinType::Unit,
            owner_class: None,
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            internal: f.internal,
            overrides: None,
            throws: f.throws,
            ctor: None,
            vtable_index: None,
            initialized: false,
            kind: kind,
        };

        if let Err(sym) = self.ctxt.add_fct_to_sym(fct) {
            report(self.ctxt, f.name, f.pos, sym);
        }
    }
}

fn report(ctxt: &Context, name: Name, pos: Position, sym: Sym) {
    let name = ctxt.interner.str(name).to_string();

    let msg = if sym.is_class() {
        Msg::ShadowClass(name)
    } else {
        Msg::ShadowFunction(name)
    };

    ctxt.diag.borrow_mut().report(pos, msg);
}
