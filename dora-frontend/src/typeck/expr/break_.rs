use crate::SourceType;
use crate::args;
use crate::error::diagnostics::OUTSIDE_LOOP;
use crate::sema::ExprId;
use crate::typeck::TypeCheck;

pub(crate) fn check_expr_break(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.in_loop {
        ck.report(ck.expr_span(expr_id), &OUTSIDE_LOOP, args!());
    }

    SourceType::Unit
}
