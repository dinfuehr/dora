use std::collections::HashSet;

use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{FctParent, Sema, SourceFileId, TypeParamDefinition, TypeParamId};
use crate::{
    parse_type, parse_type_bound, ErrorMessage, ModuleSymTable, SourceType, SourceTypeArray,
    SymbolKind,
};

pub fn parse_definitions(sa: &Sema) {
    check_traits(sa);
    check_impls(sa);
    check_classes(sa);
    check_enums(sa);
    check_structs(sa);
    check_extensions(sa);

    check_fct(sa);
}

fn check_traits(sa: &Sema) {
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

fn check_impls(sa: &Sema) {
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

fn check_classes(sa: &Sema) {
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

fn check_enums(sa: &Sema) {
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

fn check_structs(sa: &Sema) {
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

fn check_extensions(sa: &Sema) {
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

fn check_fct(sa: &Sema) {
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

        let container_type_params = type_param_definition.len();
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
    where_bounds: Option<&ast::Where>,
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

            if !ty.is_error() {
                assert!(ty.is_trait());
                if !type_param_definition.add_bound(id, ty) {
                    let msg = ErrorMessage::DuplicateTraitBound;
                    sa.report(file_id, type_param.span, msg);
                }
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
                    type_param_definition.add_where_bound(ty.clone(), bound_ty);
                }
            }
        }
    }
}
