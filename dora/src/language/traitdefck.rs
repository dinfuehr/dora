use dora_parser::ast;

use crate::language;
use crate::language::sem_analysis::{
    FctDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId, TraitDefinition,
    TraitDefinitionId,
};
use crate::language::sym::NestedSymTable;
use crate::language::ty::{SourceType, SourceTypeArray};

pub fn check(sa: &SemAnalysis) {
    for trait_ in sa.traits.iter() {
        let (trait_id, file_id, ast, module_id) = {
            let trait_ = trait_.read();
            (
                trait_.id(),
                trait_.file_id,
                trait_.ast.clone(),
                trait_.module_id,
            )
        };

        let trait_ = &sa.traits[trait_id];
        let mut trait_ = trait_.write();

        let mut traitck = TraitCheck {
            sa,
            trait_id,
            file_id,
            ast: &ast,
            module_id,
            trait_: &mut *trait_,
            sym: NestedSymTable::new(sa, module_id),
            vtable_index: 0,
        };

        traitck.check();
    }
}

struct TraitCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: SourceFileId,
    trait_id: TraitDefinitionId,
    ast: &'x ast::Trait,
    module_id: ModuleDefinitionId,
    trait_: &'x mut TraitDefinition,
    sym: NestedSymTable,
    vtable_index: u32,
}

impl<'x> TraitCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        }

        self.sym.pop_level();

        let methods = self.trait_.methods.clone();

        for method_id in methods {
            self.visit_method(method_id);
        }
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let type_params = language::check_type_params(
            self.sa,
            ast_type_params,
            &mut self.trait_.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );

        self.trait_.ty = SourceType::Trait(self.trait_id, SourceTypeArray::with(type_params));
    }

    fn visit_method(&mut self, fct_id: FctDefinitionId) {
        let fct = self.sa.fcts.idx(fct_id);
        let mut fct = fct.write();

        fct.vtable_index = Some(self.vtable_index);
        self.vtable_index += 1;

        let table = if fct.is_static {
            &mut self.trait_.static_names
        } else {
            &mut self.trait_.instance_names
        };

        if !table.contains_key(&fct.name) {
            table.insert(fct.name, fct_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fn foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fn foo() { self.bar(); } }",
            pos(1, 32),
            SemError::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fn foo(): Int32 { return false; } }",
            pos(1, 31),
            SemError::ReturnType("Int32".into(), "Bool".into()),
        );
    }

    #[test]
    fn trait_definitions() {
        ok("trait Foo {}");
        ok("trait Foo { fn toBool(): Bool; }");
        ok("trait Foo {
                fn toFloat32(): Float32;
                fn toFloat64(): Float64;
            }");

        err(
            "trait Bar { fn foo(): Unknown; }",
            pos(1, 23),
            SemError::UnknownIdentifier("Unknown".into()),
        );
        err(
            "trait Foo { fn foo(); fn foo(): Int32; }",
            pos(1, 23),
            SemError::MethodExists("foo".into(), pos(1, 13)),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            pos(1, 23),
            SemError::MethodExists("foo".into(), pos(1, 13)),
        );
    }

    #[test]
    fn trait_with_self() {
        err(
            "trait Foo {
            fn foo(): Int32;
            fn foo(): Self;
        }",
            pos(3, 13),
            SemError::MethodExists("foo".into(), pos(2, 13)),
        );
    }
}
