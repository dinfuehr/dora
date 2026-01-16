use dora_parser::ast::{self, SyntaxNodeBase};

use super::lit::check_expr_lit_str;
use crate::args;
use crate::error::diagnostics::EXPECTED_STRINGABLE;
use crate::sema::{ExprId, TemplateExpr, find_impl, implements_trait};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::{SourceType, SourceTypeArray};

pub(super) fn check_expr_template(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    node: ast::AstTemplateExpr,
    _sema_expr: &TemplateExpr,
    expected_ty: SourceType,
) -> SourceType {
    let stringable_trait_id = ck.sa.known.traits.stringable();
    let stringable_trait_ty = TraitType::from_trait_id(stringable_trait_id);

    for (idx, part_expr) in node.parts().enumerate() {
        if idx % 2 != 0 {
            let part_ty = check_expr(ck, part_expr.clone(), SourceType::Any);

            if part_ty.is_error() {
                continue;
            }

            if implements_trait(
                ck.sa,
                part_ty.clone(),
                ck.element,
                stringable_trait_ty.clone(),
            ) {
                if !part_ty.is_type_param() {
                    let impl_match = find_impl(
                        ck.sa,
                        ck.element,
                        part_ty.clone(),
                        &ck.type_param_definition,
                        stringable_trait_ty.clone(),
                    )
                    .expect("missing impl");
                    let stringable_impl_id = impl_match.id;

                    let name = ck.sa.interner.intern("toString");
                    let stringable_trait = &ck.sa.trait_(stringable_trait_id);
                    let trait_to_string_id = stringable_trait
                        .get_method(name, false)
                        .expect("missing method");

                    let to_string_id = ck
                        .sa
                        .impl_(stringable_impl_id)
                        .get_method_for_trait_method_id(trait_to_string_id)
                        .expect("missing method");

                    ck.body
                        .insert_template(part_expr.id(), (to_string_id, impl_match.bindings));
                }
            } else {
                let ty = ck.ty_name(&part_ty);
                ck.report(part_expr.span(), &EXPECTED_STRINGABLE, args![ty]);
            }
        } else {
            let e = part_expr.as_lit_str_expr();
            let expr_id = ck.expr_id(e.id());
            let text = ck.expr(expr_id).as_lit_str();
            check_expr_lit_str(ck, expr_id, text, expected_ty.clone());
        }
    }

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.body.set_ty(node.id(), str_ty.clone());

    str_ty
}
