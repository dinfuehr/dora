use crate::sema::{
    new_identity_type_params, AliasParent, Element, FctDefinition, FctParent, Sema, SourceFileId,
    TypeParamDefinition,
};
use crate::{parsety, ModuleSymTable, ParsedType, SourceType, SymbolKind};

pub fn parse_types(sa: &Sema) {
    parse_trait_types(sa);
    parse_impl_types(sa);
    parse_class_types(sa);
    parse_enum_types(sa);
    parse_struct_types(sa);
    parse_extension_types(sa);
    parse_function_types(sa);
    parse_global_types(sa);
    parse_const_types(sa);
    parse_alias_types(sa);
}

fn parse_alias_types(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        let mut table = ModuleSymTable::new(sa, alias.module_id);
        table.push_level();

        parse_type_param_definition(
            sa,
            alias.type_param_definition(),
            &mut table,
            alias.file_id,
            alias,
            false,
        );

        for bound in &alias.bounds {
            parsety::parse_trait_bound_type(
                sa,
                &table,
                alias.file_id,
                alias,
                false,
                bound.parsed_ty(),
            );
        }

        let allow_self = match alias.parent {
            AliasParent::Impl(..) | AliasParent::Trait(..) => true,
            AliasParent::None => false,
        };

        if let Some(parsed_ty) = alias.parsed_ty() {
            parsety::parse_type(sa, &table, alias.file_id, alias, allow_self, parsed_ty);
        }

        table.pop_level();
    }
}

fn parse_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        let symtable = ModuleSymTable::new(sa, const_.module_id);

        parsety::parse_type(
            sa,
            &symtable,
            const_.file_id,
            const_,
            false,
            const_.parsed_ty(),
        );
    }
}

fn parse_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        let symtable = ModuleSymTable::new(sa, global.module_id);

        parsety::parse_type(
            sa,
            &symtable,
            global.file_id,
            global,
            false,
            global.parsed_ty(),
        );
    }
}

fn parse_trait_types(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let mut symtable = ModuleSymTable::new(sa, trait_.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            trait_.type_param_definition(),
            &mut symtable,
            trait_.file_id,
            trait_,
            true,
        );

        symtable.pop_level();
    }
}

fn parse_impl_types(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let mut symtable = ModuleSymTable::new(sa, impl_.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            impl_.type_param_definition(),
            &mut symtable,
            impl_.file_id,
            impl_,
            true,
        );

        parsety::parse_trait_type(
            sa,
            &symtable,
            impl_.file_id,
            impl_,
            true,
            impl_.parsed_trait_ty(),
        );

        parsety::parse_type(
            sa,
            &symtable,
            impl_.file_id.into(),
            impl_,
            false,
            impl_.parsed_extended_ty(),
        );

        symtable.pop_level();
    }
}

fn parse_class_types(sa: &Sema) {
    for (cls_id, cls) in sa.classes.iter() {
        let mut symtable = ModuleSymTable::new(sa, cls.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            cls.type_param_definition(),
            &mut symtable,
            cls.file_id(),
            cls,
            false,
        );

        let number_type_params = cls.type_param_definition().type_param_count();
        cls.ty
            .set(SourceType::Class(
                cls_id,
                new_identity_type_params(number_type_params),
            ))
            .expect("already initialized");

        for field in &cls.fields {
            parsety::parse_type(sa, &symtable, cls.file_id(), cls, false, field.parsed_ty());
        }

        symtable.pop_level();
    }
}

fn parse_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let mut symtable = ModuleSymTable::new(sa, enum_.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            enum_.type_param_definition(),
            &mut symtable,
            enum_.file_id,
            enum_,
            false,
        );

        for variant in &enum_.variants {
            for parsed_ty in &variant.parsed_types {
                parsety::parse_type(sa, &symtable, enum_.file_id, enum_, false, parsed_ty);
            }
        }

        symtable.pop_level();
    }
}

fn parse_struct_types(sa: &Sema) {
    for (_struct_id, struct_) in sa.structs.iter() {
        let mut symtable = ModuleSymTable::new(sa, struct_.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            struct_.type_param_definition(),
            &mut symtable,
            struct_.file_id,
            struct_,
            false,
        );

        for field in &struct_.fields {
            parsety::parse_type(
                sa,
                &symtable,
                struct_.file_id,
                struct_,
                false,
                field.parsed_ty(),
            );
        }

        symtable.pop_level();
    }
}

fn parse_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        let mut symtable = ModuleSymTable::new(sa, extension.module_id);
        symtable.push_level();

        parse_type_param_definition(
            sa,
            extension.type_param_definition(),
            &mut symtable,
            extension.file_id,
            extension,
            true,
        );

        parsety::parse_type(
            sa,
            &symtable,
            extension.file_id,
            extension,
            false,
            extension.parsed_ty(),
        );

        symtable.pop_level();
    }
}

fn parse_function_types(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        parse_type_param_definition(
            sa,
            fct.type_param_definition(),
            &mut sym_table,
            fct.file_id,
            fct,
            fct.is_self_allowed(),
        );

        let allow_self = fct.is_self_allowed();

        for p in fct.params_with_self() {
            parsety::parse_type(sa, &sym_table, fct.file_id, fct, allow_self, p.parsed_ty());
        }

        parsety::parse_type(
            sa,
            &sym_table,
            fct.file_id,
            fct,
            allow_self,
            fct.parsed_return_type(),
        );

        sym_table.pop_level();
    }
}

fn parse_type_param_definition(
    sa: &Sema,
    type_param_definition: &TypeParamDefinition,
    symtable: &mut ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
) {
    for (id, name) in type_param_definition.names() {
        if symtable.get(name).is_none() {
            let old = symtable.insert(name, SymbolKind::TypeParam(id));
            assert!(old.is_none());
        }
    }

    for bound in type_param_definition.own_bounds() {
        parsety::parse_type(
            sa,
            &symtable,
            file_id,
            element,
            allow_self,
            bound.parsed_ty(),
        );
        parsety::parse_trait_bound_type(
            sa,
            &symtable,
            file_id,
            element,
            allow_self,
            bound.parsed_trait_ty(),
        );
    }
}

pub fn check_types(sa: &Sema) {
    check_trait_types(sa);
    check_impl_types(sa);
    check_alias_types(sa);
    check_struct_types(sa);
    check_class_types(sa);
    check_enum_types(sa);
    check_extension_types(sa);
    check_function_types(sa);
    check_global_types(sa);
    check_const_types(sa);
}

fn check_alias_types(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: alias.module_id,
            file_id: alias.file_id,
            type_param_definition: alias.type_param_definition(),
        };

        check_type_param_definition(sa, alias, &ctxt, alias.type_param_definition());

        if let Some(parsed_ty) = alias.parsed_ty() {
            parsety::check_type(sa, alias, &ctxt, parsed_ty);
        }

        for bound in alias.bounds() {
            parsety::check_trait_type(sa, alias, &ctxt, bound.parsed_ty());
        }
    }
}

fn check_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: const_.module_id,
            file_id: const_.file_id,
            type_param_definition: &TypeParamDefinition::empty(),
        };
        parsety::check_type(sa, const_, &ctxt, const_.parsed_ty());
    }
}

fn check_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: global.module_id,
            file_id: global.file_id,
            type_param_definition: &TypeParamDefinition::empty(),
        };
        parsety::check_type(sa, global, &ctxt, global.parsed_ty());
    }
}

fn check_trait_types(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: true,
            module_id: trait_.module_id,
            file_id: trait_.file_id,
            type_param_definition: trait_.type_param_definition(),
        };

        check_type_param_definition(sa, trait_, &ctxt, trait_.type_param_definition());
    }
}

fn check_struct_types(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: struct_.module_id,
            file_id: struct_.file_id,
            type_param_definition: struct_.type_param_definition(),
        };

        check_type_param_definition(sa, struct_, &ctxt, struct_.type_param_definition());

        for field in &struct_.fields {
            parsety::check_type(sa, struct_, &ctxt, field.parsed_ty());
        }
    }
}

fn check_class_types(sa: &Sema) {
    for (_id, class) in sa.classes.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: class.module_id,
            file_id: class.file_id(),
            type_param_definition: class.type_param_definition(),
        };

        check_type_param_definition(sa, class, &ctxt, class.type_param_definition());

        for field in &class.fields {
            parsety::check_type(sa, class, &ctxt, field.parsed_ty());
        }
    }
}

fn check_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: enum_.module_id,
            file_id: enum_.file_id,
            type_param_definition: enum_.type_param_definition(),
        };

        check_type_param_definition(sa, enum_, &ctxt, enum_.type_param_definition());

        for variant in enum_.variants() {
            for parsed_ty in variant.parsed_types() {
                parsety::check_type(sa, enum_, &ctxt, parsed_ty);
            }
        }
    }
}

fn check_impl_types(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let ctxt: parsety::TypeContext<'_> = parsety::TypeContext {
            allow_self: false,
            module_id: impl_.module_id,
            file_id: impl_.file_id,
            type_param_definition: impl_.type_param_definition(),
        };

        check_type_param_definition(sa, impl_, &ctxt, impl_.type_param_definition());
        parsety::check_type(sa, impl_, &ctxt, impl_.parsed_extended_ty());

        let mut ctxt = ctxt;
        ctxt.allow_self = true;
        parsety::check_trait_type(sa, impl_, &ctxt, impl_.parsed_trait_ty());
    }
}

fn check_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        let ctxt: parsety::TypeContext<'_> = parsety::TypeContext {
            allow_self: false,
            module_id: extension.module_id,
            file_id: extension.file_id,
            type_param_definition: extension.type_param_definition(),
        };

        check_type_param_definition(sa, extension, &ctxt, extension.type_param_definition());

        parsety::check_type(sa, extension, &ctxt, extension.parsed_ty());
    }
}

fn check_function_types(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let ctxt: parsety::TypeContext<'_> = parsety::TypeContext {
            allow_self: fct.is_self_allowed(),
            module_id: fct.module_id,
            file_id: fct.file_id,
            type_param_definition: fct.type_param_definition(),
        };

        check_type_param_definition(sa, fct, &ctxt, fct.type_param_definition());

        for param in fct.params_with_self() {
            parsety::check_type(sa, fct, &ctxt, param.parsed_ty());
        }

        parsety::check_type(sa, fct, &ctxt, fct.parsed_return_type());
    }
}

fn check_type_param_definition(
    sa: &Sema,
    element: &dyn Element,
    ctxt: &parsety::TypeContext,
    type_param_definition: &TypeParamDefinition,
) {
    for bound in type_param_definition.own_bounds() {
        parsety::check_type(sa, element, &ctxt, bound.parsed_ty());
        parsety::check_trait_type(sa, element, &ctxt, bound.parsed_trait_ty());
    }
}

pub fn expand_types(sa: &Sema) {
    expand_extension_types(sa);
    expand_impl_types(sa);
    expand_trait_types(sa);
    expand_alias_types(sa);
    expand_class_types(sa);
    expand_struct_types(sa);
    expand_enum_types(sa);
    expand_const_types(sa);
    expand_global_types(sa);
    expand_function_types(sa);
}

fn expand_impl_types(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        parsety::expand_type(sa, impl_, impl_.parsed_extended_ty(), None);
        parsety::expand_trait_type(
            sa,
            impl_,
            impl_.parsed_trait_ty(),
            Some(impl_.extended_ty()),
        );

        expand_type_param_definition(sa, impl_, impl_.type_param_definition(), None);
    }
}

fn expand_trait_types(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        expand_type_param_definition(
            sa,
            trait_,
            trait_.type_param_definition(),
            Some(SourceType::This),
        );
    }
}

fn expand_alias_types(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        expand_type_param_definition(sa, alias, alias.type_param_definition(), None);

        if let Some(parsed_ty) = alias.parsed_ty() {
            parsety::expand_type(sa, alias, parsed_ty, None);
        }
    }
}

fn expand_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        parsety::expand_type(sa, extension, extension.parsed_ty(), Some(extension.ty()));
        expand_type_param_definition(
            sa,
            extension,
            extension.type_param_definition(),
            Some(extension.ty()),
        );
    }
}

fn expand_function_types(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        for p in fct.params_with_self() {
            expand_function_type(sa, fct, p.parsed_ty());
        }

        expand_function_type(sa, fct, fct.parsed_return_type());
    }
}

fn expand_function_type(sa: &Sema, fct: &FctDefinition, parsed_ty: &ParsedType) {
    let replace_self = match fct.parent {
        FctParent::Impl(id) => {
            let impl_ = sa.impl_(id);
            Some(impl_.extended_ty())
        }
        FctParent::Extension(id) => {
            let ext = sa.extension(id);
            Some(ext.ty().clone())
        }

        FctParent::Trait(..) => Some(SourceType::This),
        FctParent::None => None,
        FctParent::Function => unreachable!(),
    };

    parsety::expand_type(sa, fct, &parsed_ty, replace_self);
}

fn expand_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        parsety::expand_type(sa, global, global.parsed_ty(), None);
    }
}

fn expand_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        parsety::expand_type(sa, const_, const_.parsed_ty(), None);
    }
}

fn expand_class_types(sa: &Sema) {
    for (_id, cls) in sa.classes.iter() {
        for field in &cls.fields {
            parsety::expand_type(sa, cls, field.parsed_ty(), None);
        }

        expand_type_param_definition(sa, cls, cls.type_param_definition(), None);
    }
}

fn expand_struct_types(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        for field in &struct_.fields {
            parsety::expand_type(sa, struct_, field.parsed_ty(), None);
        }

        expand_type_param_definition(sa, struct_, struct_.type_param_definition(), None);
    }
}

fn expand_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        for variant in enum_.variants() {
            for parsed_ty in variant.parsed_types() {
                parsety::expand_type(sa, enum_, parsed_ty, None);
            }
        }

        expand_type_param_definition(sa, enum_, enum_.type_param_definition(), None);
    }
}

fn expand_type_param_definition(
    sa: &Sema,
    element: &dyn Element,
    type_param_definition: &TypeParamDefinition,
    replace_self: Option<SourceType>,
) {
    for bound in type_param_definition.own_bounds() {
        parsety::expand_type(sa, element, bound.parsed_ty(), replace_self.clone());
        parsety::expand_trait_type(sa, element, bound.parsed_trait_ty(), replace_self.clone());
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
