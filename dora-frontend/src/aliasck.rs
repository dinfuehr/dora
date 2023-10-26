use crate::error::msg::ErrorMessage;
use crate::sema::AliasParent;
use crate::Sema;

pub fn check(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None | AliasParent::Impl(..) => {
                if alias.node.ty.is_none() {
                    sa.report(
                        alias.file_id,
                        alias.node.span,
                        ErrorMessage::TypeAliasMissingType,
                    )
                }
            }
            AliasParent::Trait(..) => {
                if alias.node.ty.is_some() {
                    sa.report(
                        alias.file_id,
                        alias.node.span,
                        ErrorMessage::UnexpectedTypeAliasAssignment,
                    )
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn type_alias() {
        ok("type Foo = Int64;");
        err("type Foo;", (1, 1), ErrorMessage::TypeAliasMissingType);
    }

    #[test]
    fn type_alias_in_impl() {
        ok("
            class Foo
            trait Bar { type X; }
            impl Bar for Foo {
                type X = Int64;
            }
        ");

        err(
            "
            class Foo
            trait Bar { type X; }
            impl Bar for Foo {
                type X;
            }
        ",
            (5, 17),
            ErrorMessage::TypeAliasMissingType,
        );
    }

    #[test]
    fn type_alias_in_trait() {
        ok("trait Foo { type Bar; }");
        err(
            "trait Foo { type Bar = Int64; }",
            (1, 13),
            ErrorMessage::UnexpectedTypeAliasAssignment,
        );
    }
}
