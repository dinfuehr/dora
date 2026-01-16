use crate::args;
use crate::error::diagnostics::OUTSIDE_LOOP;
use crate::sema::ExprId;
use crate::typeck::TypeCheck;
use crate::{SourceType, Span};

pub(crate) fn check_expr_break(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    span: Span,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.in_loop {
        ck.report(span, &OUTSIDE_LOOP, args!());
    }

    SourceType::Unit
}
