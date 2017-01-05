use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use ast::*;
use ast::visit::*;
use class::{self, ClassId};
use ctxt::*;
use error::msg::Msg;
use interner::Name;
use lexer::position::Position;
use sym::Sym::{self, SymClass, SymFct, SymStruct};
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut Context<'ast>,
                   map_cls_defs: &mut NodeMap<ClassId>,
                   map_struct_defs: &mut NodeMap<StructId>) {
    let mut gdef = GlobalDef {
        ctxt: ctxt,
        map_cls_defs: map_cls_defs,
        map_struct_defs: map_struct_defs,
    };

    gdef.visit_ast(ctxt.ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    ctxt: &'x mut Context<'ast>,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_struct_defs: &'x mut NodeMap<StructId>,
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
            primary_ctor: c.primary_ctor,

            ctors: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            ast: Some(c),
            size: 0,
            vtable: None,
        };

        self.ctxt.classes.push(Box::new(cls));
        let sym = SymClass(id);

        self.map_cls_defs.insert(c.id, id);

        if let Some(sym) = self.ctxt.sym.borrow_mut().insert(c.name, sym) {
            report(self.ctxt, c.name, c.pos, sym);
        }
    }

    fn visit_struct(&mut self, s: &'ast Struct) {
        let id: StructId = (self.ctxt.structs.len() as u32).into();
        let struc = StructData {
            id: id,
            pos: s.pos,
            name: s.name,
            fields: Vec::new(),
            size: 0,
            align: 0,
        };

        self.ctxt.structs.push(RefCell::new(struc));
        let sym = SymStruct(id);

        self.map_struct_defs.insert(s.id, id);

        if let Some(sym) = self.ctxt.sym.borrow_mut().insert(s.name, sym) {
            report(self.ctxt, s.name, s.pos, sym);
        }
    }

    fn visit_fct(&mut self, f: &'ast Function) {
        let kind = if f.block.is_some() {
            FctKind::Source(Arc::new(Mutex::new(FctSrc::new())))
        } else {
            FctKind::Definition
        };

        let fct = Fct {
            id: FctId(0),
            pos: f.pos,
            ast: f,
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
            ctor: CtorType::None,
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

    let msg = match sym {
        SymClass(_) => Msg::ShadowClass(name),
        SymStruct(_) => Msg::ShadowStruct(name),
        SymFct(_) => Msg::ShadowFunction(name),
        _ => unimplemented!(),
    };

    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use semck::tests::*;

    #[test]
    fn test_struct() {
        use error::msg::Msg;

        ok("struct Foo {}");
        err("struct Foo {} struct Foo {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()));
        err("struct Foo {} class Foo {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()));
        err("struct Foo {} fun Foo() {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()));
    }
}
