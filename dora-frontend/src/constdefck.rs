use crate::sema::Sema;
use crate::{parsety, AliasReplacement};

pub fn check(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        parsety::expand_parsed_type(
            sa,
            const_.parsed_ty(),
            None,
            AliasReplacement::ReplaceWithActualType,
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            (1, 10),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );

        ok("const x: Int32 = 0i32;");
    }

    #[test]
    fn const_with_alias_type() {
        ok("
            type MyInt = Int64;
            const x: MyInt = 10;
        ")
    }
}
