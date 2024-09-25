use crate::error::msg::ErrorMessage;
use crate::sema::{implements_trait, Sema, SourceFileId, TypeParamDefinition};
use crate::specialize::specialize_type;
use crate::sym::{ModuleSymTable, SymTable, SymbolKind};
use crate::{parsety, ParsedType};
use crate::{AliasReplacement, SourceType, SourceTypeArray};
use std::rc::Rc;

use dora_parser::ast::{self, TypeRegularType};
use dora_parser::Span;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AllowSelf {
    Yes,
    No,
}

pub fn read_type_path(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    basic: &TypeRegularType,
) -> Result<Option<SymbolKind>, ()> {
    let names = &basic.path.names;

    if names.len() > 1 {
        let first_name = sa
            .interner
            .intern(&names.first().cloned().unwrap().name_as_string);
        let last_name = sa
            .interner
            .intern(&names.last().cloned().unwrap().name_as_string);
        let mut module_table = table_for_module(sa, file_id, basic, table.get(first_name))?;

        for ident in &names[1..names.len() - 1] {
            let name = sa.interner.intern(&ident.name_as_string);
            let sym = module_table.get(name);
            module_table = table_for_module(sa, file_id, basic, sym)?;
        }

        let sym = module_table.get(last_name);
        Ok(sym)
    } else {
        let name = &names.last().cloned().unwrap().name_as_string;
        Ok(table.get_string(sa, name))
    }
}

fn table_for_module(
    sa: &Sema,
    file_id: SourceFileId,
    basic: &TypeRegularType,
    sym: Option<SymbolKind>,
) -> Result<Rc<SymTable>, ()> {
    match sym {
        Some(SymbolKind::Module(module_id)) => Ok(sa.module(module_id).table()),

        _ => {
            let msg = ErrorMessage::ExpectedModule;
            sa.report(file_id, basic.span, msg);
            Err(())
        }
    }
}

pub fn check_type_params(
    sa: &Sema,
    tp_definitions: &TypeParamDefinition,
    type_params: &[SourceType],
    file_id: SourceFileId,
    span: Span,
    type_param_defs: &TypeParamDefinition,
) -> bool {
    if tp_definitions.len() != type_params.len() {
        let msg = ErrorMessage::WrongNumberTypeParams(tp_definitions.len(), type_params.len());
        sa.report(file_id, span, msg);
        return false;
    }

    let type_params_sta = SourceTypeArray::with(type_params.to_vec());

    let mut success = true;

    for bound in tp_definitions.bounds() {
        let tp_ty = bound.ty();
        let trait_ty = bound.trait_ty();
        let tp_ty = specialize_type(sa, tp_ty, &type_params_sta);

        if !implements_trait(sa, tp_ty.clone(), type_param_defs, trait_ty.clone()) {
            let name = tp_ty.name_with_type_params(sa, type_param_defs);
            let trait_name = trait_ty.name_with_type_params(sa, type_param_defs);
            let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
            sa.report(file_id, span, msg);
            success = false;
        }
    }

    success
}

pub fn parse_type_bound(
    sa: &Sema,
    symtable: &ModuleSymTable,
    file_id: SourceFileId,
    bound: &ast::TypeData,
) -> Box<ParsedType> {
    let ty = parsety::parse_type(sa, &symtable, file_id, bound);
    let ty = ParsedType::new_ast(ty);
    parsety::convert_parsed_type2(sa, &ty);

    if !ty.is_trait() && !ty.is_error() {
        let msg = ErrorMessage::BoundExpected;
        sa.report(file_id, bound.span(), msg);
    }

    ty
}

pub fn expand_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    t: &ast::TypeData,
    type_param_defs: &TypeParamDefinition,
    allow_self: AllowSelf,
) -> Box<ParsedType> {
    let parsed_ty = parsety::parse_type(sa, table, file_id, t);
    parsety::convert_parsed_type(sa, &parsed_ty);

    let ctxt = parsety::TypeContext {
        allow_self: allow_self == AllowSelf::Yes,
        module_id: table.module_id(),
        file_id,
        type_param_defs,
    };
    parsety::check_parsed_type(sa, &ctxt, &parsed_ty);

    parsety::expand_parsed_type(
        sa,
        &parsed_ty,
        None,
        AliasReplacement::ReplaceWithActualType,
    );
    Box::new(ParsedType::Ast(*parsed_ty))
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn module_class() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub class Foo }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { class Foo }
        ",
            (2, 21),
            ErrorMessage::NotAccessible,
        );
    }

    #[test]
    fn mod_enum() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub enum Foo { A, B } }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { enum Foo { A, B } }
        ",
            (2, 21),
            ErrorMessage::NotAccessible,
        );
    }

    #[test]
    fn mod_trait() {
        ok("
            fn f(x: foo::Foo) {}
            mod foo { pub trait Foo {} }
        ");

        err(
            "
            fn f(x: foo::Foo) {}
            mod foo { trait Foo {} }
        ",
            (2, 21),
            ErrorMessage::NotAccessible,
        );
    }
}
