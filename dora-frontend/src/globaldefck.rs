use crate::args;
use crate::error::diagnostics::LET_MISSING_INITIALIZATION;
use crate::sema::Sema;

pub fn check<'a>(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        if !global.has_initial_value() {
            sa.report(
                global.file_id,
                global.span,
                &LET_MISSING_INITIALIZATION,
                args!(),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::UNKNOWN_IDENTIFIER;
    use crate::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; let mut b: Int32 = a + 1i32;");
        err(
            "let mut a: Int32 = foo;",
            (1, 20),
            3,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("foo"),
        );
    }

    #[test]
    fn check_type() {
        err(
            "let mut x: Foo = 0;",
            (1, 12),
            3,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Foo"),
        );
    }

    #[test]
    fn alias_in_global() {
        ok("
            type MyInt = Int64;
            let mut x: MyInt = 10;
        ");
    }
}
