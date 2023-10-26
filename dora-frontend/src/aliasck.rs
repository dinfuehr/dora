use crate::sema::{AliasParent, TypeParamDefinition};
use crate::{read_type, AllowSelf, ErrorMessage, ModuleSymTable, Sema, SourceType};

pub fn check(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None | AliasParent::Impl(..) => {
                if let Some(ref ty_node) = alias.node.ty {
                    let table = ModuleSymTable::new(sa, alias.module_id);
                    let ty = read_type(
                        sa,
                        &table,
                        alias.file_id,
                        ty_node,
                        &TypeParamDefinition::new(),
                        AllowSelf::No,
                    )
                    .unwrap_or(SourceType::Error);

                    assert!(alias.ty.set(ty).is_ok());
                } else {
                    sa.report(
                        alias.file_id,
                        alias.node.span,
                        ErrorMessage::TypeAliasMissingType,
                    );
                    assert!(alias.ty.set(SourceType::Error).is_ok());
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
    fn use_type_alias() {
        ok("
            type Foo = Int64;
            fn foo(x: Foo) {}
        ");
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
