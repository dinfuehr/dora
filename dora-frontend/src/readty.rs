use crate::error::msg::ErrorMessage;
use crate::sema::{implements_trait, Sema, SourceFileId, TypeParamDefinition};
use crate::specialize::specialize_type;
use crate::sym::{ModuleSymTable, SymTable, SymbolKind};
use crate::{SourceType, SourceTypeArray};
use std::rc::Rc;

use dora_parser::ast;
use dora_parser::Span;

pub fn read_type_path(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    basic: &ast::TypeRegularType,
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
    basic: &ast::TypeRegularType,
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
    if tp_definitions.type_param_count() != type_params.len() {
        let msg = ErrorMessage::WrongNumberTypeParams(
            tp_definitions.type_param_count(),
            type_params.len(),
        );
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
