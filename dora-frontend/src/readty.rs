#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::NOT_ACCESSIBLE;
    use crate::tests::*;

    #[test]
    fn module_class() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub class Foo }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { class Foo }
        ",
            (2, 21),
            8,
            crate::ErrorLevel::Error,
            &NOT_ACCESSIBLE,
            args!(),
        );
    }

    #[test]
    fn mod_enum() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub enum Foo { A, B } }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { enum Foo { A, B } }
        ",
            (2, 21),
            8,
            crate::ErrorLevel::Error,
            &NOT_ACCESSIBLE,
            args!(),
        );
    }

    #[test]
    fn mod_trait() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub trait Foo {} }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { trait Foo {} }
        ",
            (2, 21),
            8,
            crate::ErrorLevel::Error,
            &NOT_ACCESSIBLE,
            args!(),
        );
    }
}
