use std::collections::HashSet;

use dora_parser::ast;
use dora_parser::Span;

use crate::sema::ModuleDefinitionId;
use crate::sema::{FctParent, Sema, SourceFileId, TypeParamDefinition, TypeParamId};
use crate::verify_type;
use crate::{
    parse_type, parse_type_bound, AllowSelf, ErrorMessage, ModuleSymTable, SourceType,
    SourceTypeArray, SymbolKind,
};

pub fn parse_types(sa: &Sema) {
    parse_trait_type_params(sa);
    parse_impl_type_params(sa);
    parse_class_type_params(sa);
    parse_enum_type_params(sa);
    parse_struct_type_params(sa);
    parse_extension_type_params(sa);
    parse_fct_type_params(sa);
}

fn parse_trait_type_params(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let mut symtable = ModuleSymTable::new(sa, trait_.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            trait_.ast.type_params.as_ref(),
            trait_.ast.where_bounds.as_ref(),
            &mut symtable,
            trait_.file_id,
            trait_.span,
        );

        symtable.pop_level();
        assert!(trait_.type_params.set(type_param_definition).is_ok());
    }
}

fn parse_impl_type_params(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let mut symtable = ModuleSymTable::new(sa, impl_.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            impl_.ast.type_params.as_ref(),
            impl_.ast.where_bounds.as_ref(),
            &mut symtable,
            impl_.file_id,
            impl_.span,
        );

        symtable.pop_level();

        assert!(impl_.type_params.set(type_param_definition).is_ok());
    }
}

fn parse_class_type_params(sa: &Sema) {
    for (cls_id, cls) in sa.classes.iter() {
        let mut symtable = ModuleSymTable::new(sa, cls.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            cls.ast().type_params.as_ref(),
            cls.ast().where_bounds.as_ref(),
            &mut symtable,
            cls.file_id(),
            cls.span(),
        );

        symtable.pop_level();

        let number_type_params = type_param_definition.len();
        cls.type_params
            .set(type_param_definition)
            .expect("already initialized");
        cls.ty
            .set(SourceType::Class(
                cls_id,
                build_type_params(number_type_params),
            ))
            .expect("already initialized");
    }
}

fn parse_enum_type_params(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let mut symtable = ModuleSymTable::new(sa, enum_.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            enum_.ast.type_params.as_ref(),
            enum_.ast.where_bounds.as_ref(),
            &mut symtable,
            enum_.file_id,
            enum_.span,
        );

        symtable.pop_level();

        assert!(enum_.type_params.set(type_param_definition).is_ok());
    }
}

fn parse_struct_type_params(sa: &Sema) {
    for (_struct_id, struct_) in sa.structs.iter() {
        let mut symtable = ModuleSymTable::new(sa, struct_.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            struct_.ast.type_params.as_ref(),
            struct_.ast.where_bounds.as_ref(),
            &mut symtable,
            struct_.file_id,
            struct_.span,
        );

        symtable.pop_level();

        struct_
            .type_params
            .set(type_param_definition)
            .expect("already initialized");
    }
}

fn parse_extension_type_params(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        let mut symtable = ModuleSymTable::new(sa, extension.module_id);
        symtable.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            extension.ast.type_params.as_ref(),
            extension.ast.where_bounds.as_ref(),
            &mut symtable,
            extension.file_id,
            extension.span,
        );

        symtable.pop_level();

        assert!(extension.type_params.set(type_param_definition).is_ok());
    }
}

fn parse_fct_type_params(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        let mut sym_table = ModuleSymTable::new(sa, fct.module_id);
        sym_table.push_level();

        let mut type_param_definition = TypeParamDefinition::new();

        match fct.parent {
            FctParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);
                type_param_definition.append(impl_.type_params());

                for &alias_id in impl_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = sa.extension(extension_id);
                type_param_definition.append(extension.type_params());
            }

            FctParent::Trait(trait_id) => {
                let trait_ = sa.trait_(trait_id);
                type_param_definition.append(&trait_.type_params());

                for &alias_id in trait_.aliases() {
                    let alias = sa.alias(alias_id);
                    sym_table.insert(alias.name, SymbolKind::TypeAlias(alias_id));
                }
            }

            FctParent::None => {}

            FctParent::Function => unreachable!(),
        }

        let container_type_params = type_param_definition.set_container_type_params();
        assert!(fct.container_type_params.set(container_type_params).is_ok());

        read_type_param_definition(
            sa,
            &mut type_param_definition,
            fct.ast.type_params.as_ref(),
            fct.ast.where_bounds.as_ref(),
            &mut sym_table,
            fct.file_id,
            fct.span,
        );

        sym_table.pop_level();

        assert!(fct.type_params.set(type_param_definition).is_ok());
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
    type_param_definition: &mut TypeParamDefinition,
    ast_type_params: Option<&ast::TypeParams>,
    where_bounds: Option<&ast::WhereBounds>,
    symtable: &mut ModuleSymTable,
    file_id: SourceFileId,
    span: Span,
) {
    if ast_type_params.is_none() {
        return;
    }

    let ast_type_params = ast_type_params.expect("type params expected");

    if ast_type_params.params.len() == 0 {
        let msg = ErrorMessage::TypeParamsExpected;
        sa.report(file_id, span, msg);
        return;
    }

    let mut names = HashSet::new();
    let container_type_params = type_param_definition.len();

    // 1) Discover all type parameters.

    for type_param in ast_type_params.params.iter() {
        if let Some(ref ident) = type_param.name {
            let iname = sa.interner.intern(&ident.name_as_string);

            if !names.insert(iname) {
                let name = ident.name_as_string.clone();
                let msg = ErrorMessage::TypeParamNameNotUnique(name);
                sa.report(file_id, type_param.span, msg);
            }

            let id = type_param_definition.add_type_param(iname);

            let sym = SymbolKind::TypeParam(id);
            symtable.insert(iname, sym);
        } else {
            let name = sa.interner.intern("<missing name>");
            type_param_definition.add_type_param(name);
        }
    }

    assert_eq!(
        type_param_definition.len(),
        ast_type_params.params.len() + container_type_params
    );

    // 2) Read bounds for type parameters.

    for (id, type_param) in ast_type_params.params.iter().enumerate() {
        let id = TypeParamId(container_type_params + id);

        for bound in &type_param.bounds {
            let ty = parse_type_bound(sa, &symtable, file_id, bound);

            if ty.is_trait() {
                type_param_definition.add_bound(id, ty, bound.clone());
            }
        }
    }

    // 3) Read bounds in where clauses.

    if let Some(where_bounds) = where_bounds {
        for clause in where_bounds.clauses.iter() {
            let ty = parse_type(sa, &symtable, file_id, &clause.ty);

            for bound in &clause.bounds {
                let bound_ty = parse_type_bound(sa, &symtable, file_id, bound);

                if !bound_ty.is_error() {
                    assert!(bound_ty.is_trait());
                    type_param_definition.add_where_bound(
                        ty.clone(),
                        clause.ty.clone(),
                        bound_ty,
                        bound.clone(),
                    );
                }
            }
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
            trait_.type_params(),
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
            struct_.type_params(),
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
            class.type_params(),
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
            enum_.type_params(),
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
            impl_.type_params(),
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
            extension.type_params(),
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
    type_params: &TypeParamDefinition,
    allow_self: AllowSelf,
) {
    for bound in type_params.bounds() {
        if let Some(ref ty_ast) = bound.ty_ast {
            verify_type(
                sa,
                module_id,
                file_id,
                ty_ast,
                bound.ty(),
                type_params,
                allow_self,
            );
        }

        verify_type(
            sa,
            module_id,
            file_id,
            &bound.type_bound_ast,
            bound.trait_ty(),
            type_params,
            allow_self,
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn fct_with_invalid_type_bound() {
        err(
            "
            trait Foo {}
            trait Bar[T: Foo] { fn testme(); }
            fn f[X: Bar[Int64]](x: X) {
                x.testme();
            }
        ",
            (4, 21),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo".into()),
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
