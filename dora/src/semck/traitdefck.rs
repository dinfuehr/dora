use std::sync::Arc;

use crate::error::msg::SemError;
use crate::ty::SourceType;
use crate::vm::{Fct, FctId, FctKind, FctParent, FileId, NamespaceId, TraitId, VM};

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

        let fct = Fct {
            id: FctId(0),
            ast: node.clone(),
            pos: node.pos,
            name: node.name,
            namespace_id: self.namespace_id,
            param_types: Vec::new(),
            return_type: SourceType::Unit,
            parent: FctParent::Trait(self.trait_id),
            has_override: node.has_override,
            has_open: node.has_open,
            has_final: node.has_final,
            has_optimize_immediately: node.has_optimize_immediately,
            is_pub: node.is_pub,
            is_static: node.is_static,
            is_abstract: false,
            is_test: node.is_test,
            use_cannon: node.use_cannon,
            internal: node.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file_id: self.file_id.into(),
            variadic_arguments: false,

            type_params: Vec::new(),
            kind: FctKind::Definition,
            bytecode: None,
            intrinsic: None,
        };

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
