use crate::args;
use crate::error::diagnostics::{FIELD_ASSIGN_ON_IMMUTABLE, MUTATING_METHOD_ON_IMMUTABLE};
use crate::sema::{Expr, ExprId, IdentType};
use crate::typeck::TypeCheck;
use crate::{SourceType, ty::error as ty_error};

/// Indicates the reason for checking mutability of a value type base.
#[derive(Clone, Copy)]
pub(super) enum MutabilityCheckReason {
    FieldAssignment,
    MutatingMethodCall,
}

mod as_;
mod assign;
mod bin;
mod block;
mod break_;
pub(crate) mod call;
mod continue_;
mod field;
mod for_;
mod if_;
mod is;
mod lambda;
mod lit;
mod match_;
mod method_call;
mod paren;
mod path;
mod ref_;
mod return_;
mod template;
mod tuple;
mod un;
mod while_;

pub(crate) use self::break_::check_expr_break;
pub(crate) use self::call::{check_call_arguments, check_expr_call};
pub(crate) use self::continue_::check_expr_continue;
pub(crate) use self::for_::check_expr_for;
pub(crate) use self::if_::check_expr_if;
pub(crate) use self::match_::check_expr_match;
pub(crate) use self::method_call::{check_expr_method_call, check_method_call_arguments};
pub(crate) use self::path::{PathResolution, resolve_path};
pub(crate) use self::return_::check_expr_return;
pub(crate) use self::while_::check_expr_while;

use self::as_::check_expr_as;
use self::bin::check_expr_bin;
use self::block::check_expr_block;
use self::field::check_expr_field;
use self::is::check_expr_is;
use self::lambda::check_expr_lambda;
use self::lit::{
    check_expr_lit_bool, check_expr_lit_char, check_expr_lit_float, check_expr_lit_int,
    check_expr_lit_str,
};
use self::paren::check_expr_paren;
use self::ref_::check_expr_ref;
use self::template::check_expr_template;
use self::tuple::check_expr_tuple;
use self::un::check_expr_un;

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
) -> SourceType {
    let sema_expr = ck.expr(expr_id);

    match sema_expr {
        Expr::LitChar(text) => check_expr_lit_char(ck, expr_id, text, expected_ty),
        Expr::LitInt(text) => check_expr_lit_int(ck, expr_id, text, false, expected_ty),
        Expr::LitFloat(text) => check_expr_lit_float(ck, expr_id, text, false, expected_ty),
        Expr::LitStr(text) => check_expr_lit_str(ck, expr_id, text, expected_ty),
        Expr::Template(sema_expr) => check_expr_template(ck, expr_id, sema_expr, expected_ty),
        Expr::LitBool(..) => check_expr_lit_bool(ck, expr_id, expected_ty),
        Expr::Path(sema_expr) => self::path::check_expr_path(ck, expr_id, sema_expr, expected_ty),
        Expr::Un(sema_expr) => check_expr_un(ck, expr_id, sema_expr, expected_ty),
        Expr::Assign(sema_expr) => self::assign::check_expr_assign(ck, expr_id, sema_expr),
        Expr::Bin(sema_expr) => check_expr_bin(ck, expr_id, sema_expr, expected_ty),
        Expr::Call(sema_expr) => check_expr_call(ck, expr_id, sema_expr, expected_ty),
        Expr::Field(sema_expr) => check_expr_field(ck, expr_id, sema_expr, expected_ty),
        Expr::As(sema_expr) => check_expr_as(ck, expr_id, sema_expr, expected_ty),
        Expr::Is(sema_expr) => check_expr_is(ck, expr_id, sema_expr, expected_ty),
        Expr::Lambda(sema_expr) => check_expr_lambda(ck, expr_id, sema_expr, expected_ty),
        Expr::Block(sema_expr) => check_expr_block(ck, expr_id, sema_expr, expected_ty),
        Expr::If(sema_expr) => check_expr_if(ck, expr_id, sema_expr, expected_ty),
        Expr::Tuple(sema_expr) => check_expr_tuple(ck, expr_id, sema_expr, expected_ty),
        Expr::Paren(subexpr_id) => check_expr_paren(ck, expr_id, *subexpr_id, expected_ty),
        Expr::Match(sema_expr) => check_expr_match(ck, expr_id, sema_expr, expected_ty),
        Expr::For(sema_expr) => check_expr_for(ck, expr_id, sema_expr, expected_ty),
        Expr::While(sema_expr) => check_expr_while(ck, expr_id, sema_expr, expected_ty),
        Expr::Return(sema_expr) => check_expr_return(ck, expr_id, sema_expr, expected_ty),
        Expr::Break => check_expr_break(ck, expr_id, expected_ty),
        Expr::Continue => check_expr_continue(ck, expr_id, expected_ty),
        Expr::MethodCall(sema_expr) => check_expr_method_call(ck, expr_id, sema_expr, expected_ty),
        Expr::QualifiedPath(sema_expr) => {
            self::call::check_expr_qualified_path(ck, expr_id, sema_expr, expected_ty)
        }
        Expr::Ref(sema_expr) => check_expr_ref(ck, expr_id, sema_expr, expected_ty),
        Expr::Error => ty_error(),
    }
}

/// Traverses back through value-type field expressions (struct/tuple fields) to find
/// the base expression. Stops when encountering a class field or non-field expression.
fn find_value_type_chain_base(ck: &TypeCheck, mut expr_id: ExprId) -> ExprId {
    loop {
        let expr = ck.expr(expr_id);
        match expr {
            Expr::Paren(inner_expr) => {
                expr_id = *inner_expr;
            }
            Expr::Field(field_expr) => {
                if let Some(ident_type) = ck.body.get_ident(expr_id) {
                    match ident_type {
                        IdentType::StructField(..) | IdentType::TupleField(..) => {
                            // Value-type field, continue traversing
                            expr_id = field_expr.lhs;
                        }
                        IdentType::Field(..) => {
                            // Class field - stop here, class fields are heap-allocated
                            // so we don't need the variable to be mutable
                            return expr_id;
                        }
                        _ => return expr_id,
                    }
                } else {
                    return expr_id;
                }
            }
            _ => return expr_id,
        }
    }
}

/// For value type field assignments and mutating method calls, check that the
/// base variable is mutable. Traverses the field chain to find the base expression.
pub(super) fn check_value_type_base_mutability(
    ck: &mut TypeCheck,
    object_expr: ExprId,
    assign_expr_id: ExprId,
    reason: MutabilityCheckReason,
) {
    let base_expr = find_value_type_chain_base(ck, object_expr);

    if let Some(IdentType::Var(var_id)) = ck.body.get_ident(base_expr) {
        let nested_var_id = ck.vars.nested_var_id(var_id);

        // Check if this is the `self` parameter in a `mutating` method.
        // self is always variable 0 when is_self_available is true.
        let is_self = nested_var_id.0 == 0 && ck.is_self_available;
        let is_mutable = ck.vars.get_var(nested_var_id).mutable || (is_self && ck.is_mutating);

        if !is_mutable {
            let diagnostic = match reason {
                MutabilityCheckReason::FieldAssignment => &FIELD_ASSIGN_ON_IMMUTABLE,
                MutabilityCheckReason::MutatingMethodCall => &MUTATING_METHOD_ON_IMMUTABLE,
            };
            ck.report(ck.expr_span(assign_expr_id), diagnostic, args![]);
        }
    }
}
