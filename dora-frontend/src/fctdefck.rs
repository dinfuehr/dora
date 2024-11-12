use crate::sema::{FctDefinition, Sema};
use crate::ErrorMessage;

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        check_test(sa, &*fct);
    }
}

fn check_test(sa: &Sema, fct: &FctDefinition) {
    if !fct.is_test {
        return;
    }

    if !fct.parent.is_none()
        || !fct.type_param_definition().is_empty()
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
        err("fn f[]() {}", (1, 5), ErrorMessage::TypeParamsExpected);
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
