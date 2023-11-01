use std::collections::HashSet;

use dora_parser::ast;
use dora_parser::Span;

use crate::error::msg::ErrorMessage;
use crate::readty::read_type_raw;
use crate::sema::{Sema, SourceFileId, TypeParamDefinition, TypeParamId};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::ty::{SourceType, SourceTypeArray};

pub fn check(sa: &Sema) {
    check_traits(sa);
    check_impls(sa);
    check_classes(sa);
    check_enums(sa);
    check_structs(sa);
    check_extensions(sa);
}

fn check_traits(sa: &Sema) {
    for (_id, trait_) in sa.traits.iter() {
        let mut symtable = ModuleSymTable::new(sa, trait_.module_id);
        symtable.push_level();

        let type_param_definition = read_type_param_definition(
            sa,
            trait_.ast.type_params.as_ref(),
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

        let type_param_definition = read_type_param_definition(
            sa,
            impl_.ast.type_params.as_ref(),
            &mut symtable,
            impl_.file_id,
            impl_.span,
        );

        read_type_raw(sa, &symtable, impl_.file_id, &impl_.ast.extended_type);

        symtable.pop_level();

        assert!(impl_.type_params.set(type_param_definition).is_ok());
    }
}

fn check_classes(sa: &Sema) {
    for (cls_id, cls) in sa.classes.iter() {
        let mut symtable = ModuleSymTable::new(sa, cls.module_id);
        symtable.push_level();

        let type_param_definition = read_type_param_definition(
            sa,
            cls.ast().type_params.as_ref(),
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

        let type_param_definition = read_type_param_definition(
            sa,
            enum_.ast.type_params.as_ref(),
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
        let type_param_definition;

        {
            let mut symtable = ModuleSymTable::new(sa, struct_.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                struct_.ast.type_params.as_ref(),
                &mut symtable,
                struct_.file_id,
                struct_.span,
            );

            symtable.pop_level();
        }

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

        let type_param_definition = read_type_param_definition(
            sa,
            extension.ast.type_params.as_ref(),
            &mut symtable,
            extension.file_id,
            extension.span,
        );

        symtable.pop_level();

        assert!(extension.type_params.set(type_param_definition).is_ok());
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
    ast_type_params: Option<&ast::TypeParams>,
    symtable: &mut ModuleSymTable,
    file_id: SourceFileId,
    span: Span,
) -> TypeParamDefinition {
    if ast_type_params.is_none() {
        return TypeParamDefinition::new();
    }

    let ast_type_params = ast_type_params.expect("type params expected");

    if ast_type_params.params.len() == 0 {
        let msg = ErrorMessage::TypeParamsExpected;
        sa.report(file_id, span, msg);

        return TypeParamDefinition::new();
    }

    let mut names = HashSet::new();
    let mut result_type_params = TypeParamDefinition::new();

    // 1) Discover all type parameters.

    for (id, type_param) in ast_type_params.params.iter().enumerate() {
        let id = TypeParamId(id);
        if let Some(ref ident) = type_param.name {
            let iname = sa.interner.intern(&ident.name_as_string);

            if !names.insert(iname) {
                let name = ident.name_as_string.clone();
                let msg = ErrorMessage::TypeParamNameNotUnique(name);
                sa.report(file_id, type_param.span, msg);
            }

            let sym = SymbolKind::TypeParam(id);
            symtable.insert(iname, sym);

            result_type_params.add_type_param(iname);
        } else {
            let name = sa.interner.intern("<missing name>");
            result_type_params.add_type_param(name);
        }
    }

    // 2) Read bounds for type parameters.

    for (id, type_param) in ast_type_params.params.iter().enumerate() {
        let id = TypeParamId(id);

        for bound in &type_param.bounds {
            let ty = read_type_raw(sa, &symtable, file_id, bound);

            if ty.is_trait() {
                if !result_type_params.add_bound(id, ty) {
                    let msg = ErrorMessage::DuplicateTraitBound;
                    sa.report(file_id, type_param.span, msg);
                }
            } else if !ty.is_error() {
                let msg = ErrorMessage::BoundExpected;
                sa.report(file_id, bound.span(), msg);
            }
        }
    }

    result_type_params
}
