use std::collections::{HashMap, HashSet};

use crate::error::msg::ErrorMessage;
use crate::sema::Sema;

pub fn check(sa: &mut Sema) {
    for impl_ in sa.impls.iter() {
        let impl_for = {
            let impl_ = impl_.read();
            let trait_ = sa.traits[impl_.trait_id()].read();

            let all: HashSet<_> = trait_.methods.iter().cloned().collect();
            let mut defined = HashSet::new();
            let mut impl_for = HashMap::new();

            for &method_id in &impl_.methods {
                let method = sa.fcts.idx(method_id);
                let method = method.read();

                if let Some(fid) = trait_.find_method_with_replace(
                    sa,
                    method.is_static,
                    method.name,
                    Some(impl_.extended_ty.clone()),
                    method.params_without_self(),
                ) {
                    defined.insert(fid);
                    impl_for.insert(fid, method_id);

                    let trait_method = sa.fcts.idx(fid);
                    let trait_method = trait_method.read();

                    let return_type_valid = method.return_type
                        == if trait_method.return_type.is_self() {
                            impl_.extended_ty.clone()
                        } else {
                            trait_method.return_type.clone()
                        };

                    if !return_type_valid {
                        let impl_return_type = method.return_type.name_fct(sa, &*method);
                        let trait_return_type =
                            trait_method.return_type.name_fct(sa, &*trait_method);

                        let msg =
                            ErrorMessage::ReturnTypeMismatch(impl_return_type, trait_return_type);
                        sa.diag.lock().report(impl_.file_id, method.span, msg);
                    }
                } else {
                    let args = method
                        .params_without_self()
                        .iter()
                        .map(|a| a.name_fct(sa, &*method))
                        .collect::<Vec<String>>();
                    let mtd_name = sa.interner.str(method.name).to_string();
                    let trait_name = sa.interner.str(trait_.name).to_string();

                    let msg = if method.is_static {
                        ErrorMessage::StaticMethodNotInTrait(trait_name, mtd_name, args)
                    } else {
                        ErrorMessage::MethodNotInTrait(trait_name, mtd_name, args)
                    };

                    sa.diag.lock().report(impl_.file_id, method.span, msg)
                }
            }

            for &method_id in all.difference(&defined) {
                let method = sa.fcts.idx(method_id);
                let method = method.read();

                if method.has_body() {
                    // method has a default implementation, use that one
                    impl_for.insert(method_id, method_id);
                    continue;
                }

                let args = method
                    .params_without_self()
                    .iter()
                    .map(|a| a.name_fct(sa, &*method))
                    .collect::<Vec<String>>();
                let mtd_name = sa.interner.str(method.name).to_string();
                let trait_name = sa.interner.str(trait_.name).to_string();

                let msg = if method.is_static {
                    ErrorMessage::StaticMethodMissingFromTrait(trait_name, mtd_name, args)
                } else {
                    ErrorMessage::MethodMissingFromTrait(trait_name, mtd_name, args)
                };

                sa.diag.lock().report(impl_.file_id, impl_.span, msg)
            }

            impl_for
        };

        impl_.write().impl_for = impl_for;
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                fn bar() {}
            }",
            (5, 17),
            ErrorMessage::MethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_missing_in_impl() {
        err(
            "
            trait Foo {
                fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::MethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_returning_self() {
        ok("trait Foo {
                fn foo(): Self;
            }

            class A

            impl Foo for A {
                fn foo(): A { return A(); }
            }");
    }

    #[test]
    fn static_method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                static fn bar() {}
            }",
            (5, 24),
            ErrorMessage::StaticMethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn static_method_missing_in_impl() {
        err(
            "
            trait Foo {
                static fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::StaticMethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_return_type_check() {
        err(
            "trait X {
                fn m(): Bool;
                fn n(): Bool;
              }
              
              class CX
              
              impl X for CX {
                fn m(): Int32 { 0 }
                fn n(): Bool { true }
              }",
            (9, 17),
            ErrorMessage::ReturnTypeMismatch("Int32".into(), "Bool".into()),
        );
    }

    #[test]
    fn impl_method_with_default_body() {
        ok("
            trait Foo {
                fn foo(): Int32 { 1 }
            }
            class Bar {}
            impl Foo for Bar {}");
    }
}
