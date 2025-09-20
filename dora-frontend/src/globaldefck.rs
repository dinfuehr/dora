use crate::error::msg::ErrorMessage;
use crate::sema::Sema;

pub fn check<'a>(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        if !global.has_initial_value() {
            let msg = ErrorMessage::LetMissingInitialization;
            sa.report(global.file_id, global.span, msg);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; let mut b: Int32 = a + 1i32;");
        err(
            "let mut a: Int32 = foo;",
            (1, 20),
            ErrorMessage::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "let mut x: Foo = 0;",
            (1, 12),
            ErrorMessage::UnknownIdentifier("Foo".into()),
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
