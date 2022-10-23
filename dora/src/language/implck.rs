use std::collections::{HashMap, HashSet};

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{SemAnalysis, SourceFileId};

use dora_parser::lexer::position::Position;

pub fn check(sa: &mut SemAnalysis) {
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
                        sa.diag.lock().report(impl_.file_id, method.pos, msg);
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

                    report(sa, impl_.file_id, method.pos, msg);
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

                report(sa, impl_.file_id, impl_.pos, msg);
            }

            impl_for
        };

        impl_.write().impl_for = impl_for;
    }
}

fn report(sa: &SemAnalysis, file: SourceFileId, pos: Position, msg: ErrorMessage) {
    sa.diag.lock().report(file, pos, msg);
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                fun bar(): Unit {}
            }",
            pos(5, 17),
            ErrorMessage::MethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_missing_in_impl() {
        err(
            "
            trait Foo {
                fun bar(): Unit;
            }
            class A
            impl Foo for A {}",
            pos(6, 13),
            ErrorMessage::MethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_returning_self() {
        ok("trait Foo {
                fun foo(): Self;
            }

            class A

            impl Foo for A {
                fun foo(): A { return A(); }
            }");
    }

    #[test]
    fn static_method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                @static fun bar(): Unit {}
            }",
            pos(5, 25),
            ErrorMessage::StaticMethodNotInTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn static_method_missing_in_impl() {
        err(
            "
            trait Foo {
                @static fun bar(): Unit;
            }
            class A
            impl Foo for A {}",
            pos(6, 13),
            ErrorMessage::StaticMethodMissingFromTrait("Foo".into(), "bar".into(), vec![]),
        );
    }

    #[test]
    fn method_return_type_check() {
        err(
            "trait X {
                fun m(): Bool;
                fun n(): Bool;
              }
              
              class CX
              
              impl X for CX {
                fun m(): Int32 = 0;
                fun n(): Bool = true;
              }",
            pos(9, 17),
            ErrorMessage::ReturnTypeMismatch("Int32".into(), "Bool".into()),
        );
    }

    #[test]
    fn impl_method_with_default_body() {
        ok("
            trait Foo {
                fun foo(): Int32 { 1 }
            }
            class Bar {}
            impl Foo for Bar {}");
    }
}
