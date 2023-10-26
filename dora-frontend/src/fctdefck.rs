use std::collections::HashSet;

use crate::error::msg::ErrorMessage;
use crate::sema::{
    FctDefinition, FctDefinitionId, FctParent, Sema, TypeParamDefinition, TypeParamId,
};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::ty::SourceType;
use crate::{read_type, read_type_context, AllowSelf, TypeParamContext};

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let ast = fct.ast.clone();

        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        let mut type_params = TypeParamDefinition::new();
        let mut param_types: Vec<SourceType> = Vec::new();

        match fct.parent {
            FctParent::Impl(impl_id) => {
                let impl_ = &sa.impls[impl_id];
                type_params.append(impl_.type_params());

                if fct.has_hidden_self_argument() {
                    param_types.push(impl_.extended_ty());
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = &sa.extensions[extension_id];
                type_params.append(extension.type_params());

                if fct.has_hidden_self_argument() {
                    param_types.push(extension.ty().clone());
                }
            }

            FctParent::Trait(trait_id) => {
                let trait_ = &sa.traits[trait_id];
                type_params.append(&trait_.type_params());

                if fct.has_hidden_self_argument() {
                    param_types.push(SourceType::This);
                }
            }

            FctParent::None => {}

            FctParent::Function => unreachable!(),
        }

        for (id, name) in type_params.names() {
            sym_table.insert(name, SymbolKind::TypeParam(id));
        }

        let container_type_params = type_params.len();
        assert!(fct.container_type_params.set(container_type_params).is_ok());

        if let Some(ref ast_type_params) = ast.type_params {
            if ast_type_params.params.len() > 0 {
                let mut names = HashSet::new();

                for (type_param_id, type_param) in ast_type_params.params.iter().enumerate() {
                    let name = sa.interner.intern(
                        &type_param
                            .name
                            .as_ref()
                            .expect("missing name")
                            .name_as_string,
                    );

                    if !names.insert(name) {
                        let name = sa.interner.str(name).to_string();
                        let msg = ErrorMessage::TypeParamNameNotUnique(name);
                        sa.report(fct.file_id, type_param.span, msg);
                    }

                    type_params.add_type_param(name);

                    for bound in &type_param.bounds {
                        let ty = read_type(
                            sa,
                            &sym_table,
                            fct.file_id,
                            bound,
                            &type_params,
                            AllowSelf::No,
                        );

                        if let Some(ty) = ty {
                            if ty.is_trait() {
                                if !type_params.add_bound(
                                    TypeParamId(container_type_params + type_param_id),
                                    ty,
                                ) {
                                    let msg = ErrorMessage::DuplicateTraitBound;
                                    sa.report(fct.file_id, type_param.span, msg);
                                }
                            } else {
                                let msg = ErrorMessage::BoundExpected;
                                sa.report(fct.file_id, bound.span(), msg);
                            }
                        } else {
                            // unknown type, error is already thrown
                        }
                    }

                    let sym =
                        SymbolKind::TypeParam(TypeParamId(container_type_params + type_param_id));
                    sym_table.insert(name, sym);
                }
            } else {
                let msg = ErrorMessage::TypeParamsExpected;
                sa.report(fct.file_id, fct.span, msg);
            }
        }

        assert!(fct.type_params.set(type_params).is_ok());

        for p in &ast.params {
            if fct.is_variadic.get() {
                sa.report(
                    fct.file_id,
                    p.span,
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }

            let ty = read_type_context(
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

            param_types.push(ty);

            if p.variadic {
                fct.is_variadic.set(true);
            }
        }

        assert!(fct.param_types.set(param_types).is_ok());

        let return_type = if let Some(ret) = ast.return_type.as_ref() {
            let ty = read_type_context(
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

            ty
        } else {
            SourceType::Unit
        };

        assert!(fct.return_type.set(return_type).is_ok());
        fct.initialized.set(true);

        check_test(sa, &*fct);

        match fct.parent {
            FctParent::Impl(implid) => {
                let impl_ = &sa.impls[implid];
                check_against_methods(sa, &*fct, impl_.methods());
            }

            _ => {}
        }
    }
}

fn check_test(sa: &Sema, fct: &FctDefinition) {
    assert!(fct.initialized.get());

    if !fct.is_test {
        return;
    }

    if !fct.parent.is_none()
        || !fct.type_params().is_empty()
        || !fct.params_with_self().is_empty()
        || (!fct.return_type().is_unit() && !fct.return_type().is_error())
    {
        let msg = ErrorMessage::InvalidTestAnnotationUsage;
        sa.report(fct.file_id, fct.span, msg);
    }
}

fn check_against_methods(sa: &Sema, fct: &FctDefinition, methods: &[FctDefinitionId]) {
    for &method in methods {
        if method == fct.id() {
            continue;
        }

        let method = &sa.fcts[method];

        if method.initialized.get() && method.name == fct.name && method.is_static == fct.is_static
        {
            let method_name = sa.interner.str(method.name).to_string();

            let msg = ErrorMessage::MethodExists(method_name, method.span);
            sa.report(fct.file_id, fct.ast.span, msg);
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

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
                static fn foo() {}
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
