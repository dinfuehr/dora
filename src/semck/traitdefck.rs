use ctxt::{Fct, FctId, FctKind, FctParent, NodeMap, SemContext, TraitId};
use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use dora_parser::error::msg::Msg;
use dora_parser::lexer::position::Position;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut SemContext<'ast>, map_trait_defs: &NodeMap<TraitId>) {
    let mut clsck = TraitCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        trait_id: None,
        map_trait_defs: map_trait_defs,
    };

    clsck.check();
}

struct TraitCheck<'x, 'ast: 'x> {
    ctxt: &'x mut SemContext<'ast>,
    ast: &'ast ast::Ast,
    map_trait_defs: &'x NodeMap<TraitId>,

    trait_id: Option<TraitId>,
}

impl<'x, 'ast> TraitCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for TraitCheck<'x, 'ast> {
    fn visit_trait(&mut self, t: &'ast ast::Trait) {
        self.trait_id = Some(*self.map_trait_defs.get(t.id).unwrap());

        visit::walk_trait(self, t);

        self.trait_id = None;
    }

    fn visit_method(&mut self, f: &'ast ast::Function) {
        if self.trait_id.is_none() {
            return;
        }

        if f.block.is_some() {
            report(self.ctxt, f.pos, Msg::TraitMethodWithBody);
        }

        let fct = Fct {
            id: FctId(0),
            ast: f,
            pos: f.pos,
            name: f.name,
            param_types: Vec::new(),
            return_type: BuiltinType::Unit,
            parent: FctParent::Trait(self.trait_id.unwrap()),
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
            kind: FctKind::Definition,
        };

        let fctid = self.ctxt.add_fct(fct);

        let mut xtrait = self.ctxt.traits[self.trait_id.unwrap()].write();
        xtrait.methods.push(fctid);
    }
}

fn report(ctxt: &SemContext, pos: Position, msg: Msg) {
    ctxt.diag.lock().report_without_path(pos, msg);
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn trait_method_with_body() {
        err(
            "trait Foo { fun foo() -> Int { return 1; } }",
            pos(1, 13),
            Msg::TraitMethodWithBody,
        );
    }

    #[test]
    fn trait_definitions() {
        ok("trait Foo {}");
        ok("trait Foo { fun toBool() -> Bool; }");
        ok("trait Foo {
                fun toFloat() -> Float;
                fun toDouble() -> Double;
            }");

        err(
            "trait Bar { fun foo() -> Unknown; }",
            pos(1, 26),
            Msg::UnknownType("Unknown".into()),
        );
        err(
            "trait Foo { fun foo(); fun foo() -> Int; }",
            pos(1, 24),
            Msg::MethodExists("Foo".into(), "foo".into(), pos(1, 13)),
        );

        err(
            "trait Foo { fun foo(); fun foo(); }",
            pos(1, 24),
            Msg::MethodExists("Foo".into(), "foo".into(), pos(1, 13)),
        );
    }

    #[test]
    fn trait_with_self() {
        err(
            "trait Foo {
            fun foo() -> Int;
            fun foo() -> Self;
        }",
            pos(3, 13),
            Msg::MethodExists("Foo".into(), "foo".into(), pos(2, 13)),
        );
    }
}
