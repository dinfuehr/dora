use std::collections::HashMap;

use crate::interner::Name;
use crate::sema::{AliasDefinitionId, FctDefinitionId, Sema, TraitDefinition};

pub fn check(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let mut traitck = TraitCheck {
            sa,
            trait_,
            vtable_index: 0,
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
            alias_names: HashMap::new(),
        };

        traitck.check();

        assert!(trait_.instance_names.set(traitck.instance_names).is_ok());
        assert!(trait_.static_names.set(traitck.static_names).is_ok());
    }
}

struct TraitCheck<'x> {
    sa: &'x Sema,
    trait_: &'x TraitDefinition,
    vtable_index: u32,
    instance_names: HashMap<Name, FctDefinitionId>,
    static_names: HashMap<Name, FctDefinitionId>,
    alias_names: HashMap<Name, AliasDefinitionId>,
}

impl<'x> TraitCheck<'x> {
    fn check(&mut self) {
        for &method_id in self.trait_.methods() {
            self.visit_method(method_id);
        }

        for &alias_id in self.trait_.aliases() {
            self.visit_alias(alias_id);
        }
    }

    fn visit_method(&mut self, fct_id: FctDefinitionId) {
        let fct = &self.sa.fcts[fct_id];

        assert!(fct.vtable_index.set(self.vtable_index).is_ok());
        self.vtable_index += 1;

        let table = if fct.is_static {
            &mut self.static_names
        } else {
            &mut self.instance_names
        };

        if !table.contains_key(&fct.name) {
            table.insert(fct.name, fct_id);
        }
    }

    fn visit_alias(&mut self, alias_id: AliasDefinitionId) {
        let alias = &self.sa.aliases[alias_id];
        self.alias_names.insert(alias.name, alias_id);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fn foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fn foo() { self.bar(); } }",
            (1, 24),
            ErrorMessage::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fn foo(): Int32 { return false; } }",
            (1, 31),
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
            (1, 23),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "trait Foo { fn foo(); fn foo(): Int32; }",
            (1, 23),
            ErrorMessage::MethodExists("foo".into(), Span::new(12, 9)),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            (1, 23),
            ErrorMessage::MethodExists("foo".into(), Span::new(12, 9)),
        );
    }

    #[test]
    fn trait_with_self() {
        err(
            "trait Foo {
            fn foo(): Int32;
            fn foo(): Self;
        }",
            (3, 13),
            ErrorMessage::MethodExists("foo".into(), Span::new(24, 16)),
        );
    }
}
