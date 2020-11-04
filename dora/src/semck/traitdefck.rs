use std::sync::Arc;

use crate::error::msg::SemError;
use crate::vm::{Fct, FctParent, FileId, NamespaceId, TraitId, VM};

use dora_parser::ast;

pub fn check(vm: &VM) {
    for xtrait in &vm.traits {
        let (trait_id, file_id, ast, namespace_id) = {
            let xtrait = xtrait.read();
            (
                xtrait.id,
                xtrait.file_id,
                xtrait.ast.clone(),
                xtrait.namespace_id,
            )
        };

        let mut clsck = TraitCheck {
            vm,
            trait_id,
            file_id,
            ast: &ast,
            namespace_id,
        };

        clsck.check();
    }
}

struct TraitCheck<'x> {
    vm: &'x VM,
    file_id: FileId,
    trait_id: TraitId,
    ast: &'x ast::Trait,
    namespace_id: Option<NamespaceId>,
}

impl<'x> TraitCheck<'x> {
    fn check(&mut self) {
        for method in &self.ast.methods {
            self.visit_method(method);
        }
    }

    fn visit_method(&mut self, node: &Arc<ast::Function>) {
        if node.block.is_some() {
            self.vm.diag.lock().report(
                self.file_id.into(),
                node.pos,
                SemError::TraitMethodWithBody,
            );
        }

        let fct = Fct::new(
            self.file_id,
            self.namespace_id,
            node,
            FctParent::Trait(self.trait_id),
        );

        let fctid = self.vm.add_fct(fct);

        let mut xtrait = self.vm.traits[self.trait_id].write();
        xtrait.methods.push(fctid);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn trait_method_with_body() {
        err(
            "trait Foo { fun foo(): Int32 { return 1; } }",
            pos(1, 13),
            SemError::TraitMethodWithBody,
        );
    }

    #[test]
    fn trait_definitions() {
        ok("trait Foo {}");
        ok("trait Foo { fun toBool(): Bool; }");
        ok("trait Foo {
                fun toFloat32(): Float32;
                fun toFloat64(): Float64;
            }");

        err(
            "trait Bar { fun foo(): Unknown; }",
            pos(1, 24),
            SemError::UnknownType("Unknown".into()),
        );
        err(
            "trait Foo { fun foo(); fun foo(): Int32; }",
            pos(1, 24),
            SemError::MethodExists("foo".into(), pos(1, 13)),
        );

        err(
            "trait Foo { fun foo(); fun foo(); }",
            pos(1, 24),
            SemError::MethodExists("foo".into(), pos(1, 13)),
        );
    }

    #[test]
    fn trait_with_self() {
        err(
            "trait Foo {
            fun foo(): Int32;
            fun foo(): Self;
        }",
            pos(3, 13),
            SemError::MethodExists("foo".into(), pos(2, 13)),
        );
    }
}
