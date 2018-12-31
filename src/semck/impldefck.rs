use parking_lot::RwLock;

use ctxt::{Fct, FctId, FctKind, FctParent, FctSrc, ImplId, NodeMap, SemContext};
use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use dora_parser::error::msg::Msg;
use dora_parser::lexer::position::Position;
use sym::Sym;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut SemContext<'ast>, map_impl_defs: &NodeMap<ImplId>) {
    let mut clsck = ImplCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        impl_id: None,
        map_impl_defs: map_impl_defs,
    };

    clsck.check();
}

struct ImplCheck<'x, 'ast: 'x> {
    ctxt: &'x mut SemContext<'ast>,
    ast: &'ast ast::Ast,
    map_impl_defs: &'x NodeMap<ImplId>,

    impl_id: Option<ImplId>,
}

impl<'x, 'ast> ImplCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for ImplCheck<'x, 'ast> {
    fn visit_impl(&mut self, i: &'ast ast::Impl) {
        self.impl_id = Some(*self.map_impl_defs.get(i.id).unwrap());

        visit::walk_impl(self, i);

        let mut ximpl = self.ctxt.impls[self.impl_id.unwrap()].write();

        if let Some(Sym::SymTrait(trait_id)) = self.ctxt.sym.lock().get(i.trait_name) {
            ximpl.trait_id = Some(trait_id);
        } else {
            let name = self.ctxt.interner.str(i.trait_name).to_string();
            report(self.ctxt, i.pos, Msg::ExpectedTrait(name));
        }

        if let Some(Sym::SymClass(class_id)) = self.ctxt.sym.lock().get(i.class_name) {
            ximpl.class_id = Some(class_id);
        } else {
            let name = self.ctxt.interner.str(i.class_name).to_string();
            report(self.ctxt, i.pos, Msg::ExpectedClass(name));
        }

        if ximpl.trait_id.is_some() && ximpl.class_id.is_some() {
            let cls = self.ctxt.classes.idx(ximpl.cls_id());
            let mut cls = cls.write();
            cls.traits.push(ximpl.trait_id());
            cls.impls.push(ximpl.id);
        }

        self.impl_id = None;
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.impl_id.is_none() {
            return;
        }

        if f.block.is_none() && !f.internal {
            report(self.ctxt, f.pos, Msg::MissingFctBody);
        }

        let kind = if f.internal {
            FctKind::Definition
        } else {
            FctKind::Source(RwLock::new(FctSrc::new()))
        };

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::Impl(self.impl_id.unwrap()),
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: false,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            throws: f.throws,
            ctor: ast::CtorType::None,
            vtable_index: None,
            initialized: false,
            impl_for: None,

            type_params: Vec::new(),
            kind: kind,
        };

        let fctid = self.ctxt.add_fct(fct);

        let mut ximpl = self.ctxt.impls[self.impl_id.unwrap()].write();
        ximpl.methods.push(fctid);
    }
}

fn report(ctxt: &SemContext, pos: Position, msg: Msg) {
    ctxt.diag.lock().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fun foo() -> int;
            }
            class Bar {}
            impl Foo for Bar { fun foo() -> int;}",
            pos(6, 32),
            Msg::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fun foo() -> int;
            }
            class Bar {}
            impl Foo for Bar {
                fun foo() -> int { return 0; }
                fun foo() -> int { return 1; }
            }",
            pos(8, 17),
            Msg::MethodExists("Foo".into(), "foo".into(), pos(7, 17)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A {} impl Foo for A {}",
            pos(1, 12),
            Msg::ExpectedTrait("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            pos(1, 14),
            Msg::ExpectedClass("A".into()),
        );
    }

    #[test]
    fn impl_definitions() {
        ok("trait Foo {} class A {} impl Foo for A {}");
        ok("trait Foo { fun toBool() -> bool; }
            class A {}
            impl Foo for A { fun toBool() -> bool { return false; } }");
    }
}
