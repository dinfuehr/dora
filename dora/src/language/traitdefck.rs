use std::sync::Arc;

use crate::language;
use crate::language::sem_analysis::NamespaceId;
use crate::language::sym::NestedSymTable;
use crate::vm::{
    FctDefinition, FctParent, FileId, SemAnalysis, TraitDefinition, TraitDefinitionId,
};

use dora_parser::ast;

pub fn check(sa: &SemAnalysis) {
    for xtrait in &sa.traits {
        let (trait_id, file_id, ast, namespace_id) = {
            let xtrait = xtrait.read();
            (
                xtrait.id,
                xtrait.file_id,
                xtrait.ast.clone(),
                xtrait.namespace_id,
            )
        };

        let xtrait = &sa.traits[trait_id];
        let mut xtrait = xtrait.write();

        let mut clsck = TraitCheck {
            sa,
            trait_id,
            file_id,
            ast: &ast,
            namespace_id,
            xtrait: &mut *xtrait,
            sym: NestedSymTable::new(sa, namespace_id),
            vtable_index: 0,
        };

        clsck.check();
    }
}

struct TraitCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: FileId,
    trait_id: TraitDefinitionId,
    ast: &'x ast::Trait,
    namespace_id: NamespaceId,
    xtrait: &'x mut TraitDefinition,
    sym: NestedSymTable<'x>,
    vtable_index: u32,
}

impl<'x> TraitCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        }

        self.sym.pop_level();

        for method in &self.ast.methods {
            self.visit_method(method);
        }
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        language::check_type_params(
            self.sa,
            ast_type_params,
            &mut self.xtrait.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn visit_method(&mut self, node: &Arc<ast::Function>) {
        let mut fct = FctDefinition::new(
            self.sa,
            self.file_id,
            self.namespace_id,
            node,
            FctParent::Trait(self.trait_id),
        );

        fct.vtable_index = Some(self.vtable_index);
        self.vtable_index += 1;

        let fctid = self.sa.add_fct(fct);

        self.xtrait.methods.push(fctid);

        let table = if node.is_static {
            &mut self.xtrait.static_names
        } else {
            &mut self.xtrait.instance_names
        };

        if !table.contains_key(&node.name) {
            table.insert(node.name, fctid);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fun foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fun foo() { self.bar(); } }",
            pos(1, 33),
            SemError::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fun foo(): Int32 { return false; } }",
            pos(1, 32),
            SemError::ReturnType("Int32".into(), "Bool".into()),
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
            SemError::UnknownIdentifier("Unknown".into()),
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
