use std::collections::HashMap;

use crate::sema::{AliasDefinitionId, FctDefinitionId, Sema, TraitDefinition};
use crate::{ErrorMessage, Name};

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
        assert!(trait_.alias_names.set(traitck.alias_names).is_ok());
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
        let fct = self.sa.fct(fct_id);

        assert!(fct.vtable_index.set(self.vtable_index).is_ok());
        self.vtable_index += 1;

        let table = if fct.is_static {
            &mut self.static_names
        } else {
            &mut self.instance_names
        };

        if let Some(&existing_id) = table.get(&fct.name) {
            let existing_fct = self.sa.fct(existing_id);
            let method_name = self.sa.interner.str(fct.name).to_string();

            self.sa.report(
                fct.file_id,
                fct.ast.span,
                ErrorMessage::AliasExists(method_name, existing_fct.span),
            );
        } else {
            assert!(table.insert(fct.name, fct_id).is_none());
        }
    }

    fn visit_alias(&mut self, alias_id: AliasDefinitionId) {
        let alias = self.sa.alias(alias_id);

        if let Some(&existing_id) = self.alias_names.get(&alias.name) {
            let existing_alias = self.sa.alias(existing_id);
            let method_name = self.sa.interner.str(alias.name).to_string();

            self.sa.report(
                alias.file_id,
                alias.node.span,
                ErrorMessage::TypeExists(method_name, existing_alias.node.span),
            );
        } else {
            self.alias_names.insert(alias.name, alias_id);
        }
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
    fn trait_method_using_another_trait_method() {
        ok("
            trait Foo {
                fn foo(): Int64;
                fn bar(): Int64 { self.foo() }
            }");
    }

    #[test]
    #[ignore]
    fn trait_method_using_another_trait_method_generic() {
        ok("
        trait Foo[T] {
            fn foo(): Int64;
            fn bar(): Int64 { self.foo() }
        }");
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
            ErrorMessage::AliasExists("foo".into(), Span::new(12, 9)),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            (1, 23),
            ErrorMessage::AliasExists("foo".into(), Span::new(12, 9)),
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
            ErrorMessage::AliasExists("foo".into(), Span::new(24, 16)),
        );
    }

    #[test]
    fn trait_with_multiple_types() {
        ok("
            trait Foo {
                type a;
                type b;
            }
        ");

        err(
            "
            trait Foo {
                type a;
                type a;
            }
        ",
            (4, 17),
            ErrorMessage::TypeExists("a".into(), Span::new(41, 7)),
        );
    }

    #[test]
    fn trait_with_where_bounds() {
        ok("
            trait MyTrait {}
            trait Foo[T] where T: MyTrait {}
        ");

        ok("
            trait MyTrait {}
            trait Foo[T] where Option[T]: MyTrait {}
        ");

        err(
            "
            trait MyTrait {}
            trait Foo[T] where F: MyTrait {}
        ",
            (3, 32),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            trait Foo[T] where T: Int64 {}
        ",
            (2, 35),
            ErrorMessage::BoundExpected,
        );
    }
}
