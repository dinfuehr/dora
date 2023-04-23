use std::collections::HashSet;

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    FctDefinition, FctDefinitionId, FctParent, SemAnalysis, TypeParamId,
};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{self, AllowSelf, TypeParamContext};

pub fn check(sa: &SemAnalysis) {
    for fct in sa.fcts.iter() {
        let mut fct = fct.write();
        let ast = fct.ast.clone();

        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        match fct.parent {
            FctParent::Impl(impl_id) => {
                let impl_ = sa.impls[impl_id].read();
                fct.type_params.append(impl_.type_params());

                if fct.has_self() {
                    fct.param_types.push(impl_.extended_ty.clone());
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = sa.extensions[extension_id].read();
                fct.type_params.append(extension.type_params());

                if fct.has_self() {
                    fct.param_types.push(extension.ty.clone());
                }
            }

            FctParent::Trait(trait_id) => {
                let trait_ = sa.traits[trait_id].read();
                fct.type_params.append(&trait_.type_params());

                if fct.has_self() {
                    fct.param_types.push(SourceType::This);
                }
            }

            FctParent::None => {}

            FctParent::Function(_) => unimplemented!(),
        }

        for (id, name) in fct.type_params.names() {
            sym_table.insert(name, Sym::TypeParam(id));
        }

        let container_type_params = fct.type_params.len();
        fct.container_type_params = container_type_params;

        if let Some(ref type_params) = ast.type_params {
            if type_params.len() > 0 {
                let mut names = HashSet::new();

                for (type_param_id, type_param) in type_params.iter().enumerate() {
                    let name = type_param.name.as_ref().expect("missing name").name;

                    if !names.insert(name) {
                        let name = sa.interner.str(name).to_string();
                        let msg = ErrorMessage::TypeParamNameNotUnique(name);
                        sa.diag.lock().report(fct.file_id, type_param.span, msg);
                    }

                    fct.type_params.add_type_param(name);

                    for bound in &type_param.bounds {
                        let ty = language::read_type(
                            sa,
                            &sym_table,
                            fct.file_id,
                            bound,
                            TypeParamContext::Fct(&*fct),
                            AllowSelf::No,
                        );

                        if let Some(ty) = ty {
                            if ty.is_trait() {
                                if !fct.type_params.add_bound(
                                    TypeParamId(container_type_params + type_param_id),
                                    ty,
                                ) {
                                    let msg = ErrorMessage::DuplicateTraitBound;
                                    sa.diag.lock().report(fct.file_id, type_param.span, msg);
                                }
                            } else {
                                let msg = ErrorMessage::BoundExpected;
                                sa.diag.lock().report(fct.file_id, bound.span(), msg);
                            }
                        } else {
                            // unknown type, error is already thrown
                        }
                    }

                    let sym = Sym::TypeParam(TypeParamId(container_type_params + type_param_id));
                    sym_table.insert(name, sym);
                }
            } else {
                let msg = ErrorMessage::TypeParamsExpected;
                sa.diag.lock().report(fct.file_id, fct.span, msg);
            }
        }

        for p in &ast.params {
            if fct.is_variadic {
                sa.diag.lock().report(
                    fct.file_id,
                    p.span,
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }

            let ty = language::read_type(
                sa,
                &sym_table,
                fct.file_id,
                &p.data_type,
                TypeParamContext::Fct(&*fct),
                if fct.in_trait() {
                    AllowSelf::Yes
                } else {
                    AllowSelf::No
                },
            )
            .unwrap_or(SourceType::Error);

            fct.param_types.push(ty);

            if p.variadic {
                fct.is_variadic = true;
            }
        }

        if let Some(ret) = ast.return_type.as_ref() {
            let ty = language::read_type(
                sa,
                &sym_table,
                fct.file_id,
                ret,
                TypeParamContext::Fct(&*fct),
                if fct.in_trait() {
                    AllowSelf::Yes
                } else {
                    AllowSelf::No
                },
            )
            .unwrap_or(SourceType::Error);

            fct.return_type = ty;
        } else {
            fct.return_type = SourceType::Unit;
        }

        fct.initialized = true;

        check_test(sa, &*fct);

        match fct.parent {
            FctParent::Trait(traitid) => {
                let trait_ = sa.traits[traitid].read();
                check_against_methods(sa, &*fct, &trait_.methods);
            }

            FctParent::Impl(implid) => {
                let impl_ = sa.impls[implid].read();
                check_against_methods(sa, &*fct, &impl_.methods);
            }

            _ => {}
        }
    }
}

fn check_test(sa: &SemAnalysis, fct: &FctDefinition) {
    debug_assert!(fct.initialized);

    if !fct.is_test {
        return;
    }

    if !fct.parent.is_none()
        || !fct.type_params.is_empty()
        || !fct.param_types.is_empty()
        || (!fct.return_type.is_unit() && !fct.return_type.is_error())
    {
        let msg = ErrorMessage::InvalidTestAnnotationUsage;
        sa.diag.lock().report(fct.file_id, fct.span, msg);
    }
}

fn check_against_methods(sa: &SemAnalysis, fct: &FctDefinition, methods: &[FctDefinitionId]) {
    for &method in methods {
        if method == fct.id() {
            continue;
        }

        let method = sa.fcts.idx(method);
        let method = method.read();

        if method.initialized && method.name == fct.name && method.is_static == fct.is_static {
            let method_name = sa.interner.str(method.name).to_string();

            let msg = ErrorMessage::MethodExists(method_name, method.span);
            sa.diag.lock().report(fct.file_id, fct.ast.span, msg);
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn self_param() {
        err(
            "fn foo(x: Self) {}",
            (1, 11),
            ErrorMessage::SelfTypeUnavailable,
        );
    }

    #[test]
    fn self_return_type() {
        err(
            "fn foo(): Self {}",
            (1, 11),
            ErrorMessage::SelfTypeUnavailable,
        );
    }

    #[test]
    fn allow_same_method_as_static_and_non_static() {
        ok("class Foo
            impl Foo {
                @static fn foo() {}
                fn foo() {}
            }");
    }

    #[test]
    fn fct_with_type_params() {
        ok("fn f[T]() {}");
        ok("fn f[X, Y]() {}");
        err(
            "fn f[T, T]() {}",
            (1, 9),
            ErrorMessage::TypeParamNameNotUnique("T".into()),
        );
        err("fn f[]() {}", (1, 1), ErrorMessage::TypeParamsExpected);
    }

    #[test]
    fn fct_with_type_param_in_annotation() {
        ok("fn f[T](val: T) {}");
    }

    #[test]
    fn lambdas() {
        ok("fn f() { || {}; }");
        ok("fn f() { |a: Int32| {}; }");
        ok("fn f() { ||: Int32 { return 2; }; }");

        err(
            "fn f() { ||: Foo { }; }",
            (1, 14),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
        err(
            "fn f() { |a: Foo| { }; }",
            (1, 14),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn generic_bounds() {
        err(
            "fn f[T: Foo]() {}",
            (1, 9),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo fn f[T: Foo]() {}",
            (1, 19),
            ErrorMessage::BoundExpected,
        );
        ok("trait Foo {} fn f[T: Foo]() {}");

        err(
            "trait Foo {}
            fn f[T: Foo + Foo]() {  }",
            (2, 18),
            ErrorMessage::DuplicateTraitBound,
        );
    }

    #[test]
    fn check_previous_defined_type_params() {
        // Type params need to be cleaned up such that the following code is an error:
        err(
            "fn f(a: T) {}",
            (1, 9),
            ErrorMessage::UnknownIdentifier("T".into()),
        );
    }
}
