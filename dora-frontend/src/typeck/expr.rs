use dora_parser::ast::{AstExpr, SyntaxNodeBase};

use crate::sema::{Expr, ExprId};
use crate::typeck::TypeCheck;
use crate::{SourceType, ty::error as ty_error};

mod as_;
mod assign;
mod bin;
mod block;
mod break_;
mod call;
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
mod return_;
mod template;
mod this;
mod tuple;
mod un;
mod while_;

pub(crate) use self::break_::check_expr_break;
pub(crate) use self::call::{check_expr_call, create_call_arguments};
pub(crate) use self::continue_::check_expr_continue;
pub(crate) use self::for_::check_expr_for;
pub(crate) use self::if_::check_expr_if;
pub use self::lit::{compute_lit_float, compute_lit_int};
pub(crate) use self::match_::check_expr_match;
pub(crate) use self::method_call::{check_expr_method_call, create_method_call_arguments};
pub(crate) use self::path::read_path;
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
use self::template::check_expr_template;
use self::this::check_expr_this;
use self::tuple::check_expr_tuple;
use self::un::check_expr_un;

pub(super) fn check_expr_opt(
    ck: &mut TypeCheck,
    expr: Option<AstExpr>,
    expected_ty: SourceType,
) -> SourceType {
    if let Some(expr) = expr {
        check_expr(ck, expr, expected_ty)
    } else {
        SourceType::Error
    }
}

pub(super) fn check_expr_id(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    expected_ty: SourceType,
) -> SourceType {
    let expr = ck.syntax_by_id::<AstExpr>(expr_id);
    let sema_expr = ck.expr(expr_id);

    match (expr, sema_expr) {
        (AstExpr::LitCharExpr(..), &Expr::LitChar(ref text)) => {
            check_expr_lit_char(ck, expr_id, text, expected_ty)
        }
        (AstExpr::LitIntExpr(..), &Expr::LitInt(ref text)) => {
            check_expr_lit_int(ck, expr_id, text, false, expected_ty)
        }
        (AstExpr::LitFloatExpr(..), &Expr::LitFloat(ref text)) => {
            check_expr_lit_float(ck, expr_id, text, false, expected_ty)
        }
        (AstExpr::LitStrExpr(..), &Expr::LitStr(ref text)) => {
            check_expr_lit_str(ck, expr_id, text, expected_ty)
        }
        (AstExpr::TemplateExpr(expr), &Expr::Template(ref sema_expr)) => {
            check_expr_template(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::LitBoolExpr(..), &Expr::LitBool(..)) => {
            check_expr_lit_bool(ck, expr_id, expected_ty)
        }
        (AstExpr::PathExpr(expr), &Expr::Name(ref sema_expr)) => {
            self::path::check_expr_path(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::UnExpr(expr), &Expr::Un(ref sema_expr)) => {
            check_expr_un(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::AssignExpr(expr), &Expr::Assign(ref sema_expr)) => {
            self::assign::check_expr_assign(ck, expr_id, expr, sema_expr)
        }
        (AstExpr::BinExpr(expr), &Expr::Bin(ref sema_expr)) => {
            check_expr_bin(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::CallExpr(expr), &Expr::Call(ref sema_expr)) => {
            check_expr_call(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::FieldExpr(expr), &Expr::Field(ref sema_expr)) => {
            check_expr_field(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::ThisExpr(..), &Expr::This) => check_expr_this(ck, expr_id, expected_ty),
        (AstExpr::AsExpr(expr), &Expr::As(ref sema_expr)) => {
            check_expr_as(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::IsExpr(..), &Expr::Is(ref sema_expr)) => {
            check_expr_is(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::LambdaExpr(expr), &Expr::Lambda(ref sema_expr)) => {
            check_expr_lambda(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::BlockExpr(..), &Expr::Block(ref sema_expr)) => {
            check_expr_block(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::IfExpr(expr), &Expr::If(ref sema_expr)) => {
            check_expr_if(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::TupleExpr(expr), &Expr::Tuple(ref sema_expr)) => {
            check_expr_tuple(ck, expr_id, expr, sema_expr, expected_ty)
        }
        (AstExpr::ParenExpr(..), &Expr::Paren(subexpr_id)) => {
            check_expr_paren(ck, expr_id, subexpr_id, expected_ty)
        }
        (AstExpr::MatchExpr(..), &Expr::Match(ref sema_expr)) => {
            check_expr_match(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::ForExpr(..), &Expr::For(ref sema_expr)) => {
            check_expr_for(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::WhileExpr(..), &Expr::While(ref sema_expr)) => {
            check_expr_while(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::ReturnExpr(..), &Expr::Return(ref sema_expr)) => {
            check_expr_return(ck, expr_id, sema_expr, expected_ty)
        }
        (AstExpr::BreakExpr(..), &Expr::Break) => check_expr_break(ck, expr_id, expected_ty),
        (AstExpr::ContinueExpr(..), &Expr::Continue) => {
            check_expr_continue(ck, expr_id, expected_ty)
        }
        (AstExpr::MethodCallExpr(..), &Expr::MethodCall(ref sema_expr)) => {
            check_expr_method_call(ck, expr_id, sema_expr, expected_ty)
        }

        (AstExpr::Error { .. }, &Expr::Error) => ty_error(),
        _ => unreachable!("mismatched AstExpr and Expr variants"),
    }
}

pub(super) fn check_expr(ck: &mut TypeCheck, expr: AstExpr, expected_ty: SourceType) -> SourceType {
    let expr_id = ck.expr_id(expr.id());
    check_expr_id(ck, expr_id, expected_ty)
}
