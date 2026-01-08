use crate::sema::{
    AliasParent, Element, FctDefinition, FctParent, Sema, TypeParamDefinition,
    new_identity_type_params,
};
use crate::{ModuleSymTable, SourceType, SymbolKind, parsety};

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

        parse_type_param_definition(sa, alias.type_param_definition(), &mut table, alias, false);

        for bound in &alias.bounds {
            parsety::parse_trait_bound_type(sa, &table, alias, false, bound.parsed_ty());
        }

        if let Some(parsed_ty) = alias.parsed_ty() {
            parsed_ty.parse(sa, &table, alias);
        }

        table.pop_level();
    }
}

fn parse_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        let symtable = ModuleSymTable::new(sa, const_.module_id);

        const_.parsed_ty().parse(sa, &symtable, const_);
    }
}

fn parse_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        let symtable = ModuleSymTable::new(sa, global.module_id);

        global.parsed_ty().parse(sa, &symtable, global);
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

        for (id, name) in impl_.type_param_definition().names() {
            if symtable.get(name).is_none() {
                let old = symtable.insert(name, SymbolKind::TypeParam(id));
                assert!(old.is_none());
            }
        }

        parsety::parse_trait_type(sa, &symtable, impl_, true, impl_.parsed_trait_ty());

        impl_.parsed_extended_ty().parse(sa, &symtable, impl_);

        for bound in impl_.type_param_definition().own_bounds() {
            bound.parsed_ty().parse(sa, &symtable, impl_);
            parsety::parse_trait_bound_type(sa, &symtable, impl_, true, bound.parsed_trait_ty());
        }

        symtable.pop_level();
    }
}

fn parse_class_types(sa: &Sema) {
    for (cls_id, cls) in sa.classes.iter() {
        let mut symtable = ModuleSymTable::new(sa, cls.module_id);
        symtable.push_level();

        parse_type_param_definition(sa, cls.type_param_definition(), &mut symtable, cls, false);

        let number_type_params = cls.type_param_definition().type_param_count();
        cls.ty
            .set(SourceType::Class(
                cls_id,
                new_identity_type_params(0, number_type_params),
            ))
            .expect("already initialized");

        for &field_id in cls.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().parse(sa, &symtable, cls);
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
            enum_,
            false,
        );

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            for &field_id in variant.field_ids() {
                let field = sa.field(field_id);
                field.parsed_ty().parse(sa, &symtable, enum_);
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
            struct_,
            false,
        );

        for &field_id in struct_.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().parse(sa, &symtable, struct_);
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
            extension,
            true,
        );

        extension.parsed_ty().parse(sa, &symtable, extension);
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
            fct,
            fct.is_self_allowed(),
        );

        for p in fct.params_with_self() {
            p.parsed_ty().parse(sa, &sym_table, fct);
        }

        fct.parsed_return_type().parse(sa, &sym_table, fct);

        sym_table.pop_level();
    }
}

fn parse_type_param_definition(
    sa: &Sema,
    type_param_definition: &TypeParamDefinition,
    symtable: &mut ModuleSymTable,
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
        bound.parsed_ty().parse(sa, &symtable, element);
        parsety::parse_trait_bound_type(
            sa,
            &symtable,
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
        let allow_self = match alias.parent {
            AliasParent::Impl(..) | AliasParent::Trait(..) => true,
            AliasParent::None => false,
        };

        check_type_param_definition(sa, alias, alias.type_param_definition(), allow_self);

        if let Some(parsed_ty) = alias.parsed_ty() {
            parsed_ty.check(sa, alias, allow_self);
        }

        for bound in alias.bounds() {
            parsety::check_trait_type(sa, alias, bound.parsed_ty());
        }
    }
}

fn check_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        const_.parsed_ty().check(sa, const_, false);
    }
}

fn check_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        global.parsed_ty().check(sa, global, false);
    }
}

fn check_trait_types(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        check_type_param_definition(sa, trait_, trait_.type_param_definition(), true);
    }
}

fn check_struct_types(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        check_type_param_definition(sa, struct_, struct_.type_param_definition(), false);

        for &field_id in struct_.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().check(sa, struct_, false);
        }
    }
}

fn check_class_types(sa: &Sema) {
    for (_id, class) in sa.classes.iter() {
        check_type_param_definition(sa, class, class.type_param_definition(), false);

        for &field_id in class.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().check(sa, class, false);
        }
    }
}

fn check_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        check_type_param_definition(sa, enum_, enum_.type_param_definition(), false);

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            for &field_id in variant.field_ids() {
                let field = sa.field(field_id);
                field.parsed_ty().check(sa, enum_, false);
            }
        }
    }
}

fn check_impl_types(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_type_param_definition(sa, impl_, impl_.type_param_definition(), true);
        impl_.parsed_extended_ty().check(sa, impl_, false);
        parsety::check_trait_type(sa, impl_, impl_.parsed_trait_ty());
    }
}

fn check_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        check_type_param_definition(sa, extension, extension.type_param_definition(), true);
        extension.parsed_ty().check(sa, extension, false);
    }
}

fn check_function_types(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let allow_self = fct.is_self_allowed();
        check_type_param_definition(sa, fct, fct.type_param_definition(), allow_self);

        for param in fct.params_with_self() {
            param.parsed_ty().check(sa, fct, allow_self);
        }

        fct.parsed_return_type().check(sa, fct, allow_self);
    }
}

fn check_type_param_definition(
    sa: &Sema,
    element: &dyn Element,
    type_param_definition: &TypeParamDefinition,
    allow_self: bool,
) {
    for bound in type_param_definition.own_bounds() {
        bound.parsed_ty().check(sa, element, allow_self);
        parsety::check_trait_type(sa, element, bound.parsed_trait_ty());
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
        impl_.parsed_extended_ty().expand(sa, impl_, None);
        parsety::expand_parsed_trait_type(
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
            parsed_ty.expand(sa, alias, None);
        }
    }
}

fn expand_extension_types(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        extension
            .parsed_ty()
            .expand(sa, extension, Some(extension.ty()));
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
        let replace_self = compute_function_replace_self(sa, fct);

        for p in fct.params_with_self() {
            p.parsed_ty().expand(sa, fct, replace_self.clone());
        }

        fct.parsed_return_type()
            .expand(sa, fct, replace_self.clone());
        expand_type_param_definition(sa, fct, fct.type_param_definition(), replace_self);
    }
}

fn compute_function_replace_self(sa: &Sema, fct: &FctDefinition) -> Option<SourceType> {
    match fct.parent {
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
    }
}

fn expand_global_types(sa: &Sema) {
    for (_id, global) in sa.globals.iter() {
        global.parsed_ty().expand(sa, global, None);
    }
}

fn expand_const_types(sa: &Sema) {
    for (_id, const_) in sa.consts.iter() {
        const_.parsed_ty().expand(sa, const_, None);
    }
}

fn expand_class_types(sa: &Sema) {
    for (_id, cls) in sa.classes.iter() {
        for &field_id in cls.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().expand(sa, cls, None);
        }

        expand_type_param_definition(sa, cls, cls.type_param_definition(), None);
    }
}

fn expand_struct_types(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        for &field_id in struct_.field_ids() {
            let field = sa.field(field_id);
            field.parsed_ty().expand(sa, struct_, None);
        }

        expand_type_param_definition(sa, struct_, struct_.type_param_definition(), None);
    }
}

fn expand_enum_types(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            for &field_id in variant.field_ids() {
                let field = sa.field(field_id);
                field.parsed_ty().expand(sa, enum_, None);
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
        bound.parsed_ty().expand(sa, element, replace_self.clone());
        parsety::expand_parsed_trait_type(
            sa,
            element,
            bound.parsed_trait_ty(),
            replace_self.clone(),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::{ErrorLevel, ErrorMessage};
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
            vec![
                (
                    (4, 21),
                    10,
                    ErrorLevel::Error,
                    ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
                ),
                (
                    (5, 17),
                    10,
                    ErrorLevel::Error,
                    ErrorMessage::UnknownMethodForTypeParam,
                ),
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
