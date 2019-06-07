use parking_lot::{Mutex, RwLock};
use std::collections::HashMap;
use std::sync::Arc;

use crate::class::{self, ClassId};
use crate::ctxt;
use crate::ctxt::*;
use crate::gc::Address;
use crate::sym::Sym::{self, SymClass, SymConst, SymFct, SymGlobal, SymStruct, SymTrait};
use crate::ty::BuiltinType;
use dora_parser::ast::visit::*;
use dora_parser::ast::*;
use dora_parser::error::msg::Msg;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check<'ast>(
    ctxt: &mut SemContext<'ast>,
    map_cls_defs: &mut NodeMap<ClassId>,
    map_struct_defs: &mut NodeMap<StructId>,
    map_trait_defs: &mut NodeMap<TraitId>,
    map_impl_defs: &mut NodeMap<ImplId>,
    map_global_defs: &mut NodeMap<GlobalId>,
    map_const_defs: &mut NodeMap<ConstId>,
) {
    let mut gdef = GlobalDef {
        ctxt: ctxt,
        map_cls_defs: map_cls_defs,
        map_struct_defs: map_struct_defs,
        map_trait_defs: map_trait_defs,
        map_impl_defs: map_impl_defs,
        map_global_defs: map_global_defs,
        map_const_defs: map_const_defs,
    };

    gdef.visit_ast(ctxt.ast);
}

struct GlobalDef<'x, 'ast: 'x> {
    ctxt: &'x mut SemContext<'ast>,
    map_cls_defs: &'x mut NodeMap<ClassId>,
    map_struct_defs: &'x mut NodeMap<StructId>,
    map_trait_defs: &'x mut NodeMap<TraitId>,
    map_impl_defs: &'x mut NodeMap<ImplId>,
    map_global_defs: &'x mut NodeMap<GlobalId>,
    map_const_defs: &'x mut NodeMap<ConstId>,
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

        self.ctxt.traits.push(RwLock::new(xtrait));
        let sym = SymTrait(id);

        self.map_trait_defs.insert(t.id, id);

        if let Some(sym) = self.ctxt.sym.lock().insert(t.name, sym) {
            report(self.ctxt, t.name, t.pos, sym);
        }
    }

    fn visit_global(&mut self, g: &'ast Global) {
        let id = {
            let mut globals = self.ctxt.globals.lock();
            let id: GlobalId = (globals.len() as u32).into();
            let global = GlobalData {
                id: id,
                ast: g,
                pos: g.pos,
                name: g.name,
                ty: BuiltinType::Unit,
                reassignable: g.reassignable,
                getter: None,
                address_init: Address::null(),
                address_value: Address::null(),
            };

            globals.push(Arc::new(Mutex::new(global)));

            id
        };

        let sym = SymGlobal(id);
        self.map_global_defs.insert(g.id, id);

        if let Some(sym) = self.ctxt.sym.lock().insert(g.name, sym) {
            report(self.ctxt, g.name, g.pos, sym);
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

        self.ctxt.impls.push(RwLock::new(ximpl));
        self.map_impl_defs.insert(i.id, id);
    }

    fn visit_const(&mut self, c: &'ast Const) {
        let id = {
            let mut consts = self.ctxt.consts.lock();
            let id: ConstId = consts.len().into();
            let xconst = ConstData {
                id: id,
                pos: c.pos,
                name: c.name,
                ty: BuiltinType::Unit,
                expr: &c.expr,
                value: ConstValue::None,
            };

            consts.push(Arc::new(Mutex::new(xconst)));

            id
        };

        self.map_const_defs.insert(c.id, id);

        let sym = SymConst(id);

        if let Some(sym) = self.ctxt.sym.lock().insert(c.name, sym) {
            report(self.ctxt, c.name, c.pos, sym);
        }
    }

    fn visit_class(&mut self, c: &'ast Class) {
        let id = {
            let mut classes = self.ctxt.classes.lock();

            let id: ClassId = classes.len().into();
            let mut cls = class::Class {
                id: id,
                name: c.name,
                pos: c.pos,
                ty: self.ctxt.cls(id),
                parent_class: None,
                has_open: c.has_open,
                is_abstract: c.is_abstract,
                internal: c.internal,
                internal_resolved: false,
                has_constructor: c.has_constructor,

                constructor: None,
                fields: Vec::new(),
                methods: Vec::new(),

                traits: Vec::new(),
                impls: Vec::new(),

                type_params: Vec::new(),
                specializations: RwLock::new(HashMap::new()),
                vtable_len: 0,

                is_array: false,
                is_str: false,
            };

            if let Some(ref type_params) = c.type_params {
                for param in type_params {
                    cls.type_params.push(ctxt::TypeParam::new(param.name));
                }
            }

            classes.push(Arc::new(RwLock::new(cls)));

            id
        };

        let sym = SymClass(id);

        self.map_cls_defs.insert(c.id, id);

        if let Some(sym) = self.ctxt.sym.lock().insert(c.name, sym) {
            report(self.ctxt, c.name, c.pos, sym);
        }
    }

    fn visit_struct(&mut self, s: &'ast Struct) {
        let id = {
            let mut structs = self.ctxt.structs.lock();
            let id: StructId = (structs.len() as u32).into();
            let struc = StructData {
                id: id,
                pos: s.pos,
                name: s.name,
                fields: Vec::new(),
                specializations: RwLock::new(HashMap::new()),
            };

            structs.push(Arc::new(Mutex::new(struc)));

            id
        };

        let sym = SymStruct(id);

        self.map_struct_defs.insert(s.id, id);

        if let Some(sym) = self.ctxt.sym.lock().insert(s.name, sym) {
            report(self.ctxt, s.name, s.pos, sym);
        }
    }

    fn visit_fct(&mut self, f: &'ast Function) {
        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
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
            is_abstract: false,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,

            type_params: Vec::new(),
            kind: kind,
        };

        if let Err(sym) = self.ctxt.add_fct_to_sym(fct) {
            report(self.ctxt, f.name, f.pos, sym);
        }
    }
}

fn report(ctxt: &SemContext, name: Name, pos: Position, sym: Sym) {
    let name = ctxt.interner.str(name).to_string();

    let msg = match sym {
        SymClass(_) => Msg::ShadowClass(name),
        SymStruct(_) => Msg::ShadowStruct(name),
        SymFct(_) => Msg::ShadowFunction(name),
        SymTrait(_) => Msg::ShadowTrait(name),
        SymGlobal(_) => Msg::ShadowGlobal(name),
        SymConst(_) => Msg::ShadowConst(name),
        _ => unimplemented!(),
    };

    ctxt.diag.lock().report_without_path(pos, msg);
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use dora_parser::error::msg::Msg;

    #[test]
    fn test_struct() {
        ok("struct Foo {}");
        err(
            "struct Foo {} struct Foo {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} class Foo {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()),
        );
        err(
            "struct Foo {} fun Foo() {}",
            pos(1, 15),
            Msg::ShadowStruct("Foo".into()),
        );
    }

    #[test]
    fn test_trait() {
        ok("trait Foo {}");
        err(
            "trait Foo {} struct Foo {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()),
        );
        err(
            "trait Foo {} class Foo {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()),
        );
        err(
            "trait Foo {} fun Foo() {}",
            pos(1, 14),
            Msg::ShadowTrait("Foo".into()),
        );
    }

    #[test]
    fn test_const() {
        ok("const foo: Int = 0;");
        err(
            "const foo: Int = 0; fun foo() {}",
            pos(1, 21),
            Msg::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int = 0; class foo {}",
            pos(1, 21),
            Msg::ShadowConst("foo".into()),
        );
        err(
            "const foo: Int = 0; struct foo {}",
            pos(1, 21),
            Msg::ShadowConst("foo".into()),
        );
    }
}
