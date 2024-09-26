use crate::sema::{Sema, TypeParamDefinition};
use crate::{parsety, AliasReplacement};

pub fn check(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: const_.module_id,
            file_id: const_.file_id,
            type_param_defs: &TypeParamDefinition::new(),
        };
        parsety::check_parsed_type2(sa, &ctxt, const_.parsed_ty());

        parsety::expand_parsed_type2(
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
