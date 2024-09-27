use crate::error::msg::ErrorMessage;
use crate::sema::{Sema, TypeParamDefinition};
use crate::{parsety, AliasReplacement};

pub fn check<'a>(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: global.module_id,
            file_id: global.file_id,
            type_param_defs: &TypeParamDefinition::new(),
        };
        parsety::check_parsed_type(sa, &ctxt, global.parsed_ty());

        parsety::expand_parsed_type(
            sa,
            global.parsed_ty(),
            None,
            AliasReplacement::ReplaceWithActualType,
        );

        if !global.has_initial_value() {
            let msg = ErrorMessage::LetMissingInitialization;
            sa.report(global.file_id, global.ast.span, msg);
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
