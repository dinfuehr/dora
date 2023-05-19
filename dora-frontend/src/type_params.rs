use std::collections::HashSet;

use dora_parser::ast;
use dora_parser::Span;

use crate::error::msg::ErrorMessage;
use crate::readty::read_type_unchecked;
use crate::sema::{Sema, SourceFileId, TypeParamDefinition, TypeParamId};
use crate::sym::{ModuleSymTable, Sym};
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
    for trait_ in sa.traits.iter() {
        let type_param_definition;

        {
            let trait_ = trait_.read();
            let mut symtable = ModuleSymTable::new(sa, trait_.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                trait_.ast.type_params.as_ref(),
                &mut symtable,
                trait_.file_id,
                trait_.span,
            );

            symtable.pop_level();
        }

        trait_.write().type_params = Some(type_param_definition);
    }
}

fn check_impls(sa: &Sema) {
    for impl_ in sa.impls.iter() {
        let type_param_definition;

        {
            let impl_ = impl_.read();
            let mut symtable = ModuleSymTable::new(sa, impl_.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                impl_.ast.type_params.as_ref(),
                &mut symtable,
                impl_.file_id,
                impl_.span,
            );

            read_type_unchecked(sa, &symtable, impl_.file_id, &impl_.ast.extended_type);

            symtable.pop_level();
        }

        impl_.write().type_params = Some(type_param_definition);
    }
}

fn check_classes(sa: &Sema) {
    for cls in sa.classes.iter() {
        let type_param_definition;

        {
            let cls = cls.read();
            let mut symtable = ModuleSymTable::new(sa, cls.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                cls.ast().type_params.as_ref(),
                &mut symtable,
                cls.file_id(),
                cls.span(),
            );

            symtable.pop_level();
        }

        let number_type_params = type_param_definition.len();
        cls.write().type_params = Some(type_param_definition);

        let cls_id = cls.read().id();
        cls.write().ty = Some(SourceType::Class(
            cls_id,
            build_type_params(number_type_params),
        ));
    }
}

fn check_enums(sa: &Sema) {
    for enum_ in sa.enums.iter() {
        let type_param_definition;

        {
            let enum_ = enum_.read();
            let mut symtable = ModuleSymTable::new(sa, enum_.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                enum_.ast.type_params.as_ref(),
                &mut symtable,
                enum_.file_id,
                enum_.span,
            );

            symtable.pop_level();
        }

        enum_.write().type_params = Some(type_param_definition);
    }
}

fn check_structs(sa: &Sema) {
    for struct_ in sa.structs.iter() {
        let type_param_definition;

        {
            let struct_ = struct_.read();
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

        struct_.write().type_params = Some(type_param_definition);
    }
}

fn check_extensions(sa: &Sema) {
    for extension in sa.extensions.iter() {
        let type_param_definition;

        {
            let extension = extension.read();
            let mut symtable = ModuleSymTable::new(sa, extension.module_id);
            symtable.push_level();

            type_param_definition = read_type_param_definition(
                sa,
                extension.ast.type_params.as_ref(),
                &mut symtable,
                extension.file_id,
                extension.span,
            );

            symtable.pop_level();
        }

        extension.write().type_params = Some(type_param_definition);
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
        sa.diag.lock().report(file_id, span, msg);

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
                sa.diag.lock().report(file_id, type_param.span, msg);
            }

            let sym = Sym::TypeParam(id);
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
            let ty = read_type_unchecked(sa, &symtable, file_id, bound);

            if ty.is_trait() {
                if !result_type_params.add_bound(id, ty) {
                    let msg = ErrorMessage::DuplicateTraitBound;
                    sa.diag.lock().report(file_id, type_param.span, msg);
                }
            } else if !ty.is_error() {
                let msg = ErrorMessage::BoundExpected;
                sa.diag.lock().report(file_id, bound.span(), msg);
            }
        }
    }

    result_type_params
}
