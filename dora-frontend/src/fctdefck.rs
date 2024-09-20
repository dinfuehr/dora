use std::sync::Arc;

use dora_parser::ast;

use crate::sema::{FctDefinition, FctParent, Sema};
use crate::{
    check_type, replace_type, AliasReplacement, AllowSelf, ErrorMessage, ModuleSymTable,
    SourceType, SymbolKind,
};

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let ast = fct.ast.clone();

        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        let mut param_types: Vec<SourceType> = Vec::new();

        for (id, name) in fct.type_params().names() {
            sym_table.insert(name, SymbolKind::TypeParam(id));
        }

        match fct.parent {
            FctParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);

                if fct.has_hidden_self_argument() {
                    param_types.push(impl_.extended_ty());
                }

                for &alias_id in impl_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = sa.extension(extension_id);

                if fct.has_hidden_self_argument() {
                    param_types.push(extension.ty().clone());
                }
            }

            FctParent::Trait(trait_id) => {
                let trait_ = sa.trait_(trait_id);

                if fct.has_hidden_self_argument() {
                    param_types.push(SourceType::This);
                }

                for &alias_id in trait_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::None => {}

            FctParent::Function => unreachable!(),
        }

        for p in &ast.params {
            if fct.is_variadic.get() {
                sa.report(
                    fct.file_id,
                    p.span,
                    ErrorMessage::VariadicParameterNeedsToBeLast,
                );
            }

            let ty = process_type(sa, fct, &sym_table, &p.data_type);

            param_types.push(ty);

            if p.variadic {
                fct.is_variadic.set(true);
            }
        }

        assert!(fct.param_types.set(param_types).is_ok());

        let return_type = if let Some(ret) = ast.return_type.as_ref() {
            process_type(sa, fct, &sym_table, ret)
        } else {
            SourceType::Unit
        };

        assert!(fct.return_type.set(return_type).is_ok());
        fct.initialized.set(true);

        check_test(sa, &*fct);
    }
}

fn process_type(
    sa: &Sema,
    fct: &FctDefinition,
    sym_table: &ModuleSymTable,
    ast: &Arc<ast::TypeData>,
) -> SourceType {
    let allow_self = if fct.is_self_allowed() {
        AllowSelf::Yes
    } else {
        AllowSelf::No
    };

    let ty = check_type(
        sa,
        &sym_table,
        fct.file_id,
        ast,
        fct.type_params(),
        allow_self,
    );

    match fct.parent {
        FctParent::Impl(id) => {
            let impl_ = sa.impl_(id);
            replace_type(
                sa,
                ty,
                None,
                Some(impl_.extended_ty()),
                AliasReplacement::ReplaceWithActualType,
            )
        }

        FctParent::None => {
            replace_type(sa, ty, None, None, AliasReplacement::ReplaceWithActualType)
        }

        FctParent::Extension(id) => {
            let ext = sa.extension(id);
            replace_type(
                sa,
                ty,
                None,
                Some(ext.ty().clone()),
                AliasReplacement::ReplaceWithActualType,
            )
        }

        FctParent::Trait(trait_id) => replace_type(
            sa,
            ty,
            None,
            None,
            AliasReplacement::ReplaceWithActualTypeKeepTrait(trait_id),
        ),

        FctParent::Function => unreachable!(),
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

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use crate::Span;

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
    fn same_method_as_static_and_non_static() {
        err(
            "
            class Foo
            impl Foo {
                static fn foo() {}
                fn foo() {}
            }
        ",
            (5, 17),
            ErrorMessage::AliasExists("foo".into(), Span::new(69, 11)),
        );
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

    #[test]
    fn fct_with_where_bounds() {
        ok("
            trait MyTrait {}
            fn f[T]() where T: MyTrait {}
        ");

        ok("
            trait MyTrait {}
            fn f[T]() where Option[T]: MyTrait {}
        ");

        err(
            "
            trait MyTrait {}
            fn f[T]() where F: MyTrait {}
        ",
            (3, 29),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            fn f[T]() where T: Int64 {}
        ",
            (2, 32),
            ErrorMessage::BoundExpected,
        );
    }
}
