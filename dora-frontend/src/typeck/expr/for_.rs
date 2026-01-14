use dora_parser::ast::{self, SyntaxNodeBase};

use super::while_::check_loop_body;
use crate::args;
use crate::error::diagnostics::TYPE_NOT_USABLE_IN_FOR_IN;
use crate::sema::{ExprId, FctDefinitionId, ForExpr, ForTypeInfo, find_impl};
use crate::ty::{self, TraitType};
use crate::typeck::{TypeCheck, check_expr, check_pattern};
use crate::{SourceType, SourceTypeArray, specialize_type};

pub(crate) fn check_expr_for(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstForExpr,
    _sema_expr: &ForExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, node.expr(), SourceType::Any);

    if object_type.is_error() {
        check_for_body(ck, node, ty::error());
        return SourceType::Unit;
    }

    if let Some((for_type_info, ret_type)) = type_supports_iterator_trait(ck, object_type.clone()) {
        // store fct ids for code generation
        ck.body.insert_for_type_info(node.id(), for_type_info);
        check_for_body(ck, node, ret_type);
        return SourceType::Unit;
    }

    if let Some(into_iterator_data) = type_supports_into_iterator_trait(ck, object_type.clone()) {
        let ret_type = if let Some((mut for_type_info, ret_type)) =
            type_supports_iterator_trait(ck, into_iterator_data.iterator_type.clone())
        {
            if let Some(iter_impl_fct_id) = into_iterator_data.iter_impl_fct_id {
                // store fct ids for code generation
                for_type_info.iter = Some((iter_impl_fct_id, into_iterator_data.bindings));
                ck.body.insert_for_type_info(node.id(), for_type_info);
            }

            ret_type
        } else {
            SourceType::Error
        };

        check_for_body(ck, node, ret_type);
        return SourceType::Unit;
    }

    let name = ck.ty_name(&object_type);
    ck.report(node.expr().span(), &TYPE_NOT_USABLE_IN_FOR_IN, args!(name));

    // set invalid error type
    check_for_body(ck, node, ty::error());
    SourceType::Unit
}

fn check_for_body(ck: &mut TypeCheck, node: ast::AstForExpr, ty: SourceType) {
    ck.symtable.push_level();
    ck.enter_block_scope();
    check_pattern(ck, node.pattern(), ty);
    let block_expr = node.block();
    check_loop_body(ck, block_expr);
    ck.leave_block_scope(node.id());
    ck.symtable.pop_level();
}

struct IntoIteratorData {
    iter_impl_fct_id: Option<FctDefinitionId>,
    bindings: SourceTypeArray,
    iterator_type: SourceType,
}

fn type_supports_into_iterator_trait(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<IntoIteratorData> {
    let into_iterator_trait_id = ck.sa.known.traits.into_iterator();
    let into_iterator_trait = ck.sa.trait_(into_iterator_trait_id);

    let iter_name = ck.sa.interner.intern("iter");
    let iterator_type_name = ck.sa.interner.intern("IteratorType");

    let iter_trait_fct_id = into_iterator_trait
        .get_method(iter_name, false)
        .expect("missing next() in trait");

    let iterator_type_trait_alias_id = into_iterator_trait
        .alias_names()
        .get(&iterator_type_name)
        .cloned()
        .expect("missing Item alias");

    let trait_ty = TraitType::from_trait_id(into_iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        object_type.clone(),
        &ck.type_param_definition,
        trait_ty,
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);

        let iter_impl_fct_id = impl_.trait_method_map().get(&iter_trait_fct_id).cloned();

        let iterator_type = if let Some(iterator_type_impl_alias_id) = impl_
            .trait_alias_map()
            .get(&iterator_type_trait_alias_id)
            .cloned()
        {
            let iterator_type_impl_alias = ck.sa.alias(iterator_type_impl_alias_id);

            specialize_type(ck.sa, iterator_type_impl_alias.ty(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        Some(IntoIteratorData {
            iter_impl_fct_id,
            bindings: impl_match.bindings,
            iterator_type,
        })
    } else {
        None
    }
}

fn type_supports_iterator_trait(
    ck: &mut TypeCheck,
    object_type: SourceType,
) -> Option<(ForTypeInfo, SourceType)> {
    let iterator_trait_id = ck.sa.known.traits.iterator();
    let iterator_trait = ck.sa.trait_(iterator_trait_id);

    let next_name = ck.sa.interner.intern("next");
    let item_name = ck.sa.interner.intern("Item");

    let next_trait_fct_id = iterator_trait
        .get_method(next_name, false)
        .expect("missing next() in trait");

    let item_trait_alias_id = iterator_trait
        .alias_names()
        .get(&item_name)
        .cloned()
        .expect("missing Item alias");

    let trait_ty = TraitType::from_trait_id(iterator_trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        object_type.clone(),
        &ck.type_param_definition,
        trait_ty,
    );

    if let Some(impl_match) = impl_match {
        let impl_ = ck.sa.impl_(impl_match.id);
        let next_impl_fct_id = impl_.trait_method_map().get(&next_trait_fct_id).cloned();

        let next_type = if let Some(next_impl_fct_id) = next_impl_fct_id {
            let next_impl_fct = ck.sa.fct(next_impl_fct_id);
            specialize_type(ck.sa, next_impl_fct.return_type(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        let value_type = if let Some(item_impl_alias_id) =
            impl_.trait_alias_map().get(&item_trait_alias_id).cloned()
        {
            let impl_alias = ck.sa.alias(item_impl_alias_id);
            specialize_type(ck.sa, impl_alias.ty(), &impl_match.bindings)
        } else {
            SourceType::Error
        };

        Some((
            ForTypeInfo {
                iter: None,
                next: next_impl_fct_id,
                iterator_type: object_type,
                next_type,
                value_type: value_type.clone(),
            },
            value_type,
        ))
    } else {
        None
    }
}
