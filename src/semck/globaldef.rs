use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use ast::*;
use ast::visit::*;
use class::{self, ClassId};
use ctxt::*;
use error::msg::Msg;
use interner::Name;
use lexer::position::Position;
use sym::Sym::{self, SymClass, SymFct, SymStruct, SymTrait};
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut Context<'ast>,
                   map_cls_defs: &mut NodeMap<ClassId>,
                   map_struct_defs: &mut NodeMap<StructId>,
                   map_trait_defs: &mut NodeMap<TraitId>,
                   map_impl_defs: &mut NodeMap<ImplId>) {
    let mut gdef = GlobalDef {
        ctxt: ctxt,
        map_cls_defs: map_cls_defs,
        map_struct_defs: map_struct_defs,
        map_trait_defs: map_trait_defs,
        map_impl_defs: map_impl_defs,
    };

    gdef.visit_ast(ctxt.ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    ctxt: &'x mut Context<'ast>,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_struct_defs: &'x mut NodeMap<StructId>,
    map_trait_defs: &'x mut NodeMap<TraitId>,
    map_impl_defs: &'x mut NodeMap<ImplId>,
}

impl<'x, 'ast> Visitor<'ast> for GlobalDef<'x, 'ast> {
    fn visit_trait(&mut self, t: &'ast Trait) {
        let id: TraitId = (self.ctxt.traits.len() as u32).into();
        let xtrait = TraitData {
            id: id,
            pos: t.pos,
            name: t.name,
            methods: Vec::new(),
        };

        self.ctxt.traits.push(RefCell::new(xtrait));
        let sym = SymTrait(id);

        self.map_trait_defs.insert(t.id, id);

        if let Some(sym) = self.ctxt.sym.borrow_mut().insert(t.name, sym) {
            report(self.ctxt, t.name, t.pos, sym);
        }
    }

    fn visit_impl(&mut self, i: &'ast Impl) {
        let id: ImplId = (self.ctxt.impls.len() as u32).into();
        let ximpl = ImplData {
            id: id,
            pos: i.pos,
            trait_id: None,
            class_id: None,
            methods: Vec::new(),
        };

        self.ctxt.impls.push(RefCell::new(ximpl));
        self.map_impl_defs.insert(i.id, id);
    }

    fn visit_class(&mut self, c: &'ast Class) {
        let id: ClassId = self.ctxt.classes.len().into();
        let cls = class::Class {
            id: id,
            name: c.name,
            pos: c.pos,
            ty: BuiltinType::Class(id),
            parent_class: None,
            has_open: c.has_open,
            internal: c.internal,
            internal_resolved: false,
            primary_ctor: c.primary_ctor,

            ctors: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            size: 0,
            vtable: None,

            type_params: Vec::new(),
            specialization_for: None,
            specializations: HashMap::new(),

            ref_fields: Vec::new(),
        };

        self.ctxt.classes.push(RefCell::new(box cls));
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
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::None,
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            is_pub: true,
            is_static: false,
            internal: f.internal,
            internal_resolved: false,
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
        SymTrait(_) => Msg::ShadowTrait(name),
        _ => unimplemented!(),
    };

    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use semck::tests::*;
    use error::msg::Msg;

    #[test]
    fn test_struct() {
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

    #[test]
    fn test_trait() {
        ok("trait Foo {}");
        err("trait Foo {} struct Foo {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()));
        err("trait Foo {} class Foo {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()));
        err("trait Foo {} fun Foo() {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()));
    }
}
