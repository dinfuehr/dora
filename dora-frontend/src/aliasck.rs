use crate::sema::{AliasParent, TypeParamDefinition};
use crate::{check_type, AllowSelf, ModuleSymTable, Sema, SourceType, SymbolKind};

pub fn check(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None => {
                if let Some(ref ty_node) = alias.node.ty {
                    let table = ModuleSymTable::new(sa, alias.module_id);
                    let ty = check_type(
                        sa,
                        &table,
                        alias.file_id,
                        ty_node,
                        &TypeParamDefinition::new(),
                        AllowSelf::No,
                    );

                    assert!(alias.ty.set(ty).is_ok());
                } else {
                    assert!(alias.ty.set(SourceType::Error).is_ok());
                }
            }

            AliasParent::Impl(impl_id) => {
                if let Some(ref ty_node) = alias.node.ty {
                    let impl_ = sa.impl_(impl_id);
                    let mut table = ModuleSymTable::new(sa, alias.module_id);
                    table.push_level();

                    for (id, name) in impl_.type_params().names() {
                        table.insert(name, SymbolKind::TypeParam(id));
                    }

                    let ty = check_type(
                        sa,
                        &table,
                        alias.file_id,
                        ty_node,
                        &TypeParamDefinition::new(),
                        AllowSelf::No,
                    );

                    table.pop_level();

                    assert!(alias.ty.set(ty).is_ok());
                } else {
                    assert!(alias.ty.set(SourceType::Error).is_ok());
                }
            }

            AliasParent::Trait(..) => {}
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

    #[test]
    fn use_alias_type_in_trait() {
        ok("
            trait Foo {
                type Bar;
                fn f(): Bar;
            }
        ")
    }
}
