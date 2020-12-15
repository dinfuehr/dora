use std::sync::Arc;

use crate::semck;
use crate::sym::NestedSymTable;
use crate::vm::{Fct, FctParent, FileId, NamespaceId, TraitData, TraitId, VM};

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

        let xtrait = &vm.traits[trait_id];
        let mut xtrait = xtrait.write();

        let mut clsck = TraitCheck {
            vm,
            trait_id,
            file_id,
            ast: &ast,
            namespace_id,
            xtrait: &mut *xtrait,
            sym: NestedSymTable::new(vm, namespace_id),
            vtable_index: 0,
        };

        clsck.check();
    }
}

struct TraitCheck<'x> {
    vm: &'x VM,
    file_id: FileId,
    trait_id: TraitId,
    ast: &'x ast::Trait,
    namespace_id: NamespaceId,
    xtrait: &'x mut TraitData,
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
        semck::check_type_params(
            self.vm,
            ast_type_params,
            &mut self.xtrait.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn visit_method(&mut self, node: &Arc<ast::Function>) {
        let mut fct = Fct::new(
            self.file_id,
            self.namespace_id,
            node,
            FctParent::Trait(self.trait_id),
        );

        fct.vtable_index = Some(self.vtable_index);
        self.vtable_index += 1;

        let fctid = self.vm.add_fct(fct);

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
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fun foo(): Int32 { return 1; } }");
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
