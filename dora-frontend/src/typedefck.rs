use crate::sema::ModuleDefinitionId;
use crate::sema::{AliasParent, FctParent, Sema, SourceFileId, TypeParamDefinition, TypeParamId};
use crate::ParsedType;
use crate::{
    parsety, AllowSelf, ErrorMessage, ModuleSymTable, SourceType, SourceTypeArray, SymbolKind,
};

pub fn parse_types(sa: &Sema) {
    parse_trait_types(sa);
    parse_impl_types(sa);
    parse_class_types(sa);
    parse_enum_types(sa);
    parse_struct_types(sa);
    parse_extension_types(sa);
    parse_fct_types(sa);
    parse_global_types(sa);
    parse_const_types(sa);
    parse_alias_types(sa);
}

fn parse_alias_types(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        let mut table = ModuleSymTable::new(sa, alias.module_id);
        table.push_level();

        match alias.parent {
            AliasParent::None => {}

            AliasParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);

                for (id, name) in impl_.type_param_definition().names() {
                    table.insert(name, SymbolKind::TypeParam(id));
                }
            }

            AliasParent::Trait(id) => {
                let trait_ = sa.trait_(id);

                for (id, name) in trait_.type_param_definition().names() {
                    table.insert(name, SymbolKind::TypeParam(id));
                }

                for bound in &alias.bounds {
                    let parsed_trait_ty =
                        parsety::parse_type(sa, &table, alias.file_id, &bound.ty_ast);
                    let parsed_trait_ty = ParsedType::new_ast(parsed_trait_ty);
                    parsety::convert_parsed_type2(sa, &parsed_trait_ty);
                    assert!(bound.ty.set(parsed_trait_ty).is_ok());

                    if !bound.parsed_ty().is_trait() && !bound.parsed_ty().is_error() {
                        let msg = ErrorMessage::BoundExpected;
                        sa.report(alias.file_id, bound.ty_ast.span(), msg);
                    }
                }
            }
        }

        let parsed_ty = if let Some(ref ast_type) = alias.node.ty {
            let parsed_ty = parsety::parse_type(sa, &table, alias.file_id, ast_type);
            let parsed_ty = ParsedType::new_ast(parsed_ty);
            parsety::convert_parsed_type2(sa, &parsed_ty);
            Some(parsed_ty)
        } else {
            None
        };

        assert!(alias.parsed_ty.set(parsed_ty).is_ok());
        table.pop_level();
    }
}

fn parse_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        let symtable = ModuleSymTable::new(sa, const_.module_id);

        let ast = &const_.ast.data_type;
        let trait_ty = parsety::parse_type(sa, &symtable, const_.file_id, ast);
        let trait_ty = ParsedType::new_ast(trait_ty);
        parsety::convert_parsed_type2(sa, &trait_ty);
        assert!(const_.ty.set(trait_ty).is_ok());
    }
}

fn parse_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        let symtable = ModuleSymTable::new(sa, global.module_id);

        let ast = &global.ast.data_type;
        let trait_ty = parsety::parse_type(sa, &symtable, global.file_id, ast);
        let trait_ty = ParsedType::new_ast(trait_ty);
        parsety::convert_parsed_type2(sa, &trait_ty);
        assert!(global.ty.set(trait_ty).is_ok());
    }
}

fn parse_trait_types(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let mut symtable = ModuleSymTable::new(sa, trait_.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            trait_.type_param_definition(),
            &mut symtable,
            trait_.file_id,
        );

        symtable.pop_level();
    }
}

fn parse_impl_types(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let mut symtable = ModuleSymTable::new(sa, impl_.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            impl_.type_param_definition(),
            &mut symtable,
            impl_.file_id,
        );

        let ast_trait_type = impl_.ast.trait_type.as_ref().unwrap();
        let trait_ty = parsety::parse_type(sa, &symtable, impl_.file_id, ast_trait_type);
        let trait_ty = ParsedType::new_ast(trait_ty);
        parsety::convert_parsed_type2(sa, &trait_ty);
        assert!(impl_.trait_ty.set(trait_ty).is_ok());

        let extended_ty = parsety::parse_type(
            sa,
            &symtable,
            impl_.file_id.into(),
            &impl_.ast.extended_type,
        );
        let extended_ty = ParsedType::new_ast(extended_ty);
        parsety::convert_parsed_type2(sa, &extended_ty);
        assert!(impl_.extended_ty.set(extended_ty).is_ok());

        symtable.pop_level();
    }
}

fn parse_class_types(sa: &Sema) {
    for (cls_id, cls) in sa.classes.iter() {
        let mut symtable = ModuleSymTable::new(sa, cls.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            cls.type_param_definition(),
            &mut symtable,
            cls.file_id(),
        );

        let number_type_params = cls.type_param_definition().len();
        cls.ty
            .set(SourceType::Class(
                cls_id,
                build_type_params(number_type_params),
            ))
            .expect("already initialized");

        for (idx, field) in cls.ast().fields.iter().enumerate() {
            let ty = parsety::parse_type(sa, &symtable, cls.file_id(), &field.data_type);
            let ty = ParsedType::new_ast(ty);
            parsety::convert_parsed_type2(sa, &ty);

            assert!(cls.fields[idx].ty.set(ty).is_ok());
        }

        symtable.pop_level();
    }
}

fn parse_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let mut symtable = ModuleSymTable::new(sa, enum_.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            enum_.type_param_definition(),
            &mut symtable,
            enum_.file_id,
        );

        assert_eq!(enum_.variants.len(), enum_.ast.variants.len());

        for (variant, ast) in enum_.variants.iter().zip(enum_.ast.variants.iter()) {
            let mut parsed_types = Vec::new();
            if let Some(ref ast_variant_types) = ast.types {
                for ast_ty in ast_variant_types {
                    let ty = parsety::parse_type(sa, &symtable, enum_.file_id, ast_ty);
                    let ty = ParsedType::new_ast(ty);
                    parsety::convert_parsed_type2(sa, &ty);

                    parsed_types.push(ty);
                }
            }
            assert!(variant.types.set(parsed_types).is_ok());
        }

        symtable.pop_level();
    }
}

fn parse_struct_types(sa: &Sema) {
    for (_struct_id, struct_) in sa.structs.iter() {
        let mut symtable = ModuleSymTable::new(sa, struct_.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            struct_.type_param_definition(),
            &mut symtable,
            struct_.file_id,
        );

        for (idx, ast_field) in struct_.ast.fields.iter().enumerate() {
            let ty = parsety::parse_type(sa, &symtable, struct_.file_id, &ast_field.data_type);
            let ty = ParsedType::new_ast(ty);
            parsety::convert_parsed_type2(sa, &ty);

            assert!(struct_.fields[idx].ty.set(ty).is_ok());
        }

        symtable.pop_level();
    }
}

fn parse_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        let mut symtable = ModuleSymTable::new(sa, extension.module_id);
        symtable.push_level();

        read_type_param_definition(
            sa,
            extension.type_param_definition(),
            &mut symtable,
            extension.file_id,
        );

        let ast_type = &extension.ast.extended_type;
        let ty = parsety::parse_type(sa, &symtable, extension.file_id, ast_type);
        let ty = ParsedType::new_ast(ty);
        parsety::convert_parsed_type2(sa, &ty);
        assert!(extension.ty.set(ty).is_ok());

        symtable.pop_level();
    }
}

fn parse_fct_types(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        match fct.parent {
            FctParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);
                type_param_definition.append(impl_.type_param_definition());

                for &alias_id in impl_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = sa.extension(extension_id);
                type_param_definition.append(extension.type_param_definition());
            }

            FctParent::Trait(trait_id) => {
                let trait_ = sa.trait_(trait_id);
                type_param_definition.append(&trait_.type_param_definition());

                for &alias_id in trait_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::None => {}

            FctParent::Function => unreachable!(),
        }

        read_type_param_definition(sa, fct.type_params(), &mut sym_table, fct.file_id);

        for p in fct.params_without_self() {
            let ast_node = p.ast.as_ref().expect("missing ast");

            let parsed_ty = parsety::parse_type(sa, &sym_table, fct.file_id, &ast_node.data_type);
            let parsed_ty = ParsedType::new_ast(parsed_ty);
            parsety::convert_parsed_type2(sa, &parsed_ty);
            assert!(p.ty.set(parsed_ty).is_ok());
        }

        if let Some(ret) = fct.ast.return_type.as_ref() {
            let parsed_ty = parsety::parse_type(sa, &sym_table, fct.file_id, ret);
            let parsed_ty = ParsedType::new_ast(parsed_ty);
            parsety::convert_parsed_type2(sa, &parsed_ty);
            assert!(fct.return_type.set(parsed_ty).is_ok());
        }

        sym_table.pop_level();
    }
}

fn build_type_params(number_type_params: usize) -> SourceTypeArray {
    let type_params = (0..number_type_params)
        .into_iter()
        .map(|id| SourceType::TypeParam(TypeParamId(id)))
        .collect::<Vec<_>>();
    SourceTypeArray::with(type_params)
}

fn read_type_param_definition(
    sa: &Sema,
    type_param_definition: &TypeParamDefinition,
    symtable: &mut ModuleSymTable,
    file_id: SourceFileId,
) {
    for (id, name) in type_param_definition.names() {
        if symtable.get(name).is_none() {
            let old = symtable.insert(name, SymbolKind::TypeParam(id));
            assert!(old.is_none());
        }
    }

    for bound in type_param_definition.bounds() {
        if let Some(ref ty) = bound.ast_ty {
            let ty = parsety::parse_type(sa, &symtable, file_id, ty);
            let ty = ParsedType::new_ast(ty);
            parsety::convert_parsed_type2(sa, &ty);
            assert!(bound.ty.set(ty).is_ok());
        }

        let ty = parsety::parse_type(sa, &symtable, file_id, &bound.ast_trait_ty);
        let ty = ParsedType::new_ast(ty);
        parsety::convert_parsed_type2(sa, &ty);
        assert!(bound.trait_ty.set(ty).is_ok());

        if !bound.parsed_trait_ty().is_trait() && !bound.parsed_trait_ty().is_error() {
            let msg = ErrorMessage::BoundExpected;
            sa.report(file_id, bound.ast_trait_ty.span(), msg);
        }
    }
}

pub fn check_type_bounds(sa: &Sema) {
    check_trait_type_bounds(sa);
    check_impl_type_bounds(sa);
    check_struct_type_bounds(sa);
    check_class_type_bounds(sa);
    check_enum_type_bounds(sa);
    check_extension_type_bounds(sa);
    check_fct_type_bounds(sa);
}

fn check_trait_type_bounds(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        check_type_param_bounds(
            sa,
            trait_.module_id,
            trait_.file_id,
            trait_.type_param_definition(),
            AllowSelf::Yes,
        );
    }
}

fn check_struct_type_bounds(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        check_type_param_bounds(
            sa,
            struct_.module_id,
            struct_.file_id,
            struct_.type_param_definition(),
            AllowSelf::No,
        );
    }
}

fn check_class_type_bounds(sa: &Sema) {
    for (_id, class) in sa.classes.iter() {
        check_type_param_bounds(
            sa,
            class.module_id,
            class.file_id(),
            class.type_param_definition(),
            AllowSelf::No,
        );
    }
}

fn check_enum_type_bounds(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        check_type_param_bounds(
            sa,
            enum_.module_id,
            enum_.file_id,
            enum_.type_param_definition(),
            AllowSelf::No,
        );
    }
}

fn check_impl_type_bounds(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_type_param_bounds(
            sa,
            impl_.module_id,
            impl_.file_id,
            impl_.type_param_definition(),
            AllowSelf::Yes,
        );
    }
}

fn check_extension_type_bounds(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        check_type_param_bounds(
            sa,
            extension.module_id,
            extension.file_id,
            extension.type_param_definition(),
            AllowSelf::No,
        );
    }
}

fn check_fct_type_bounds(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let allow_self = if fct.is_self_allowed() {
            AllowSelf::Yes
        } else {
            AllowSelf::No
        };

        check_type_param_bounds(
            sa,
            fct.module_id,
            fct.file_id,
            fct.type_params(),
            allow_self,
        );
    }
}

fn check_type_param_bounds(
    sa: &Sema,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    type_param_defs: &TypeParamDefinition,
    allow_self: AllowSelf,
) {
    for bound in type_param_defs.bounds() {
        let ctxt = parsety::TypeContext {
            allow_self: allow_self == AllowSelf::Yes,
            module_id,
            file_id,
            type_param_defs,
        };
        parsety::check_parsed_type2(sa, &ctxt, bound.parsed_ty());
        parsety::check_parsed_type2(sa, &ctxt, bound.parsed_trait_ty());
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn fct_with_invalid_type_bound() {
        errors(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            fn f[X: Bar[Int64]](x: X) {
                x.testme();
            }
        ",
            &[
                (
                    (4, 21),
                    ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
                ),
                ((5, 17), ErrorMessage::UnknownMethodForTypeParam),
            ],
        );
    }

    #[test]
    fn struct_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            struct MyType[X: Bar[Int64]] {
                field: X,
            }
        ",
            (4, 30),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }

    #[test]
    fn trait_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            trait Baz[X: Bar[Int64]] {}
        ",
            (4, 26),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }

    #[test]
    fn class_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            class Baz[X: Bar[Int64]] {}
        ",
            (4, 26),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }

    #[test]
    fn enum_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            enum Baz[X: Bar[Int64]] { A(X), B }
        ",
            (4, 25),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }

    #[test]
    fn extension_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            class Baz[T]
            impl[T: Bar[Int64]] Baz[T] {}
        ",
            (5, 21),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }

    #[test]
    fn impl_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            class Baz[T]
            impl[T: Bar[Int64]] Foo for Baz[T] {}
        ",
            (5, 21),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
        );
    }
}
