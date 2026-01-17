use dora_parser::ast;

use crate::sema::{ExprId, MethodCallExpr};
use crate::typeck::{TypeCheck, check_expr};
use crate::{SourceType, SourceTypeArray};
use dora_parser::Span;

use crate::typeck::call::check_expr_call_method;

pub(crate) fn check_expr_method_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &MethodCallExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, sema_expr.object, SourceType::Any);
    let method_name = ck.sa.interner.str(sema_expr.name).to_string();

    let type_params: SourceTypeArray = SourceTypeArray::with(
        sema_expr
            .type_params
            .iter()
            .map(|&type_ref_id| ck.read_type(type_ref_id))
            .collect(),
    );

    check_method_call_arguments(ck, sema_expr);
    let call_expr_id = expr_id;
    let expr = ck.syntax_by_id::<ast::AstMethodCallExpr>(expr_id);

    // Compute span from object to method name (for error reporting on field access)
    let object_span = ck.expr_span(sema_expr.object);
    let name_token = expr.name();
    let name_span = name_token.span();
    let callee_span = Span::new(object_span.start(), name_span.end() - object_span.start());

    check_expr_call_method(
        ck,
        call_expr_id,
        sema_expr.object,
        callee_span,
        object_type,
        method_name,
        type_params,
    )
}

pub(crate) fn check_method_call_arguments(ck: &mut TypeCheck, sema_expr: &MethodCallExpr) {
    for sema_arg in &sema_expr.args {
        let ty = check_expr(ck, sema_arg.expr, SourceType::Any);
        ck.body.set_ty(sema_arg.expr, ty);
    }
}
