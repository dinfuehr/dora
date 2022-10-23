use dora_parser::ast;

use crate::language::sem_analysis::{
    FctDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId, TraitDefinition,
    TraitDefinitionId,
};
use crate::language::sym::ModuleSymTable;

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
            sym: ModuleSymTable::new(sa, module_id),
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
    sym: ModuleSymTable,
    vtable_index: u32,
}

impl<'x> TraitCheck<'x> {
    fn check(&mut self) {
        let methods = self.trait_.methods.clone();

        for method_id in methods {
            self.visit_method(method_id);
        }
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
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fun foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fun foo(): Unit { self.bar(); } }",
            pos(1, 39),
            ErrorMessage::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fun foo(): Int32 { return false; } }",
            pos(1, 32),
            ErrorMessage::ReturnType("Int32".into(), "Bool".into()),
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
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "trait Foo { fun foo(): Unit; fun foo(): Int32; }",
            pos(1, 30),
            ErrorMessage::MethodExists("foo".into(), pos(1, 13)),
        );

        err(
            "trait Foo { fun foo(): Unit; fun foo(): Unit; }",
            pos(1, 30),
            ErrorMessage::MethodExists("foo".into(), pos(1, 13)),
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
            ErrorMessage::MethodExists("foo".into(), pos(2, 13)),
        );
    }
}
