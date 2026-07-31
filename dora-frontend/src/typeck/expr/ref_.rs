use crate::SourceType;
use crate::SymbolKind;
use crate::access::global_accessible_from;
use crate::args;
use crate::error::diagnostics::{
    NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE, REF_REQUIRES_VARIABLE_OR_FIELD,
};
use crate::sema::{Expr, ExprId, IdentType, RefExpr};
use crate::ty::error as ty_error;
use crate::typeck::TypeCheck;
use crate::typeck::expr::check_expr;
use crate::typeck::expr::path::{PathResolution, resolve_path};

pub(super) fn check_expr_ref(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &RefExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let inner_expr = ck.expr(sema_expr.expr);

    match inner_expr {
        Expr::Path(path_expr) => {
            // Check that the last segment has no type parameters
            let last_segment = path_expr
                .segments
                .last()
                .expect("path should have at least one segment");
            if !last_segment.type_params.is_empty() {
                ck.report(ck.expr_span(expr_id), &NO_TYPE_PARAMS_EXPECTED, args![]);
            }

            // Resolve the path
            let resolution = match resolve_path(ck, sema_expr.expr, path_expr, false) {
                Ok(res) => res,
                Err(()) => return ty_error(),
            };

            let (inner_ty, ident) = match resolution {
                PathResolution::Symbol(SymbolKind::Var(var_id)) => {
                    let inner_ty = ck.vars.get_var(var_id).ty.clone();
                    ck.vars.mark_used(var_id);
                    let ident = ck.maybe_allocate_in_context(var_id);
                    (inner_ty, ident)
                }
                PathResolution::Symbol(SymbolKind::Global(global_id)) => {
                    if !global_accessible_from(ck.sa, global_id, ck.module_id) {
                        ck.report(ck.expr_span(expr_id), &NOT_ACCESSIBLE, args![]);
                    }

                    let inner_ty = ck.sa.global(global_id).ty();
                    (inner_ty, IdentType::Global(global_id))
                }
                _ => {
                    ck.report(
                        ck.expr_span(expr_id),
                        &REF_REQUIRES_VARIABLE_OR_FIELD,
                        args![],
                    );
                    return ty_error();
                }
            };

            // Record the identifier type for the inner expression
            ck.body.insert_ident(sema_expr.expr, ident.clone());
            ck.body.set_ty(sema_expr.expr, inner_ty.clone());

            // Create the Ref type
            let ref_ty = SourceType::Ref(Box::new(inner_ty));
            ck.body.set_ty(expr_id, ref_ty.clone());

            // Also record that this is a ref expression pointing to a variable
            ck.body.insert_ident(expr_id, ident);

            ref_ty
        }

        Expr::Field(..) => {
            // Type-check the field expression normally
            let inner_ty = check_expr(ck, sema_expr.expr, SourceType::Any);

            if inner_ty.is_error() {
                return ty_error();
            }

            // Create the Ref type
            let ref_ty = SourceType::Ref(Box::new(inner_ty));
            ck.body.set_ty(expr_id, ref_ty.clone());

            ref_ty
        }

        _ => {
            ck.report(
                ck.expr_span(expr_id),
                &REF_REQUIRES_VARIABLE_OR_FIELD,
                args![],
            );
            ty_error()
        }
    }
}
