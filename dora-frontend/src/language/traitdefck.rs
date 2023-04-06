use crate::language::sem_analysis::{FctDefinitionId, SemAnalysis, TraitDefinition};

pub fn check(sa: &SemAnalysis) {
    for trait_ in sa.traits.iter() {
        let mut trait_ = trait_.write();

        let mut traitck = TraitCheck {
            sa,
            trait_: &mut *trait_,
            vtable_index: 0,
        };

        traitck.check();
    }
}

struct TraitCheck<'x> {
    sa: &'x SemAnalysis,
    trait_: &'x mut TraitDefinition,
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
        ok("trait Foo { fn foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fn foo() { self.bar(); } }",
            pos(1, 24),
            ErrorMessage::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fn foo(): Int32 { return false; } }",
            pos(1, 31),
            ErrorMessage::ReturnType("Int32".into(), "Bool".into()),
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
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "trait Foo { fn foo(); fn foo(): Int32; }",
            pos(1, 23),
            ErrorMessage::MethodExists("foo".into(), pos(1, 13)),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            pos(1, 23),
            ErrorMessage::MethodExists("foo".into(), pos(1, 13)),
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
            ErrorMessage::MethodExists("foo".into(), pos(2, 13)),
        );
    }
}
