use crate::error::msg::Msg;
use crate::ty::BuiltinType;
use crate::vm::{Fct, FctId, FctKind, FctParent, NodeMap, TraitId, VM};

use dora_parser::ast::visit::{self, Visitor};
use dora_parser::ast::{self, Ast};

use dora_parser::lexer::position::Position;

pub fn check<'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_trait_defs: &NodeMap<TraitId>) {
    let mut clsck = TraitCheck {
        vm: vm,
        ast: ast,
        trait_id: None,
        map_trait_defs: map_trait_defs,
        file_id: 0,
    };

    clsck.check();
}

struct TraitCheck<'x, 'ast: 'x> {
    vm: &'x mut VM<'ast>,
    ast: &'ast ast::Ast,
    map_trait_defs: &'x NodeMap<TraitId>,
    file_id: u32,

    trait_id: Option<TraitId>,
}

impl<'x, 'ast> TraitCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for TraitCheck<'x, 'ast> {
    fn visit_file(&mut self, f: &'ast ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

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
            report(self.vm, f.pos, Msg::TraitMethodWithBody);
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
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file: self.file_id.into(),

            type_params: Vec::new(),
            kind: FctKind::Definition,
        };

        let fctid = self.vm.add_fct(fct);

        let mut xtrait = self.vm.traits[self.trait_id.unwrap()].write();
        xtrait.methods.push(fctid);
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
