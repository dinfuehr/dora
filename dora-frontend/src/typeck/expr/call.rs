use crate::sema::{CallExpr, Expr, ExprId};
use crate::typeck::{TypeCheck, check_expr};
use crate::{SourceType, SourceTypeArray};

use crate::typeck::call::{check_expr_call_expr, check_expr_call_path_name, check_expr_call_sym};

pub(crate) fn check_expr_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &CallExpr,
    expected_ty: SourceType,
) -> SourceType {
    let call_expr_id = expr_id;

    let callee_expr = ck.expr(sema_expr.callee);

    match callee_expr {
        Expr::Path(name_expr) => {
            // Get type params from the last segment
            let type_params = if let Some(last_segment) = name_expr.path.last() {
                let params: Vec<SourceType> = last_segment
                    .type_params
                    .iter()
                    .map(|&ty| ck.read_type(ty))
                    .collect();
                SourceTypeArray::with(params)
            } else {
                SourceTypeArray::empty()
            };

            if name_expr.path.len() == 1 {
                // Single segment: simple identifier lookup
                let sym = ck.symtable.get(name_expr.path[0].name);

                check_expr_call_sym(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    sym,
                    type_params,
                    call_expr_id,
                )
            } else {
                // Multi-segment path
                check_expr_call_path_name(
                    ck,
                    expected_ty,
                    expr_id,
                    sema_expr.callee,
                    type_params,
                    call_expr_id,
                )
            }
        }

        _ => {
            let expr_type = check_expr(ck, sema_expr.callee, SourceType::Any);
            check_expr_call_expr(ck, expr_id, expr_type, call_expr_id)
        }
    }
}

pub(crate) fn check_call_arguments(ck: &mut TypeCheck, sema_expr: &CallExpr) {
    for sema_arg in &sema_expr.args {
        let ty = check_expr(ck, sema_arg.expr, SourceType::Any);
        ck.body.set_ty(sema_arg.expr, ty);
    }
}

#[cfg(test)]
mod tests {
    use crate::error::diagnostics::UNKNOWN_IDENTIFIER;
    use crate::{args, tests::*};

    #[test]
    fn infer_enum_constructor_arg_from_expected() {
        ok("
            enum Maybe[T] { Some(T) }
            fn take(value: Maybe[Int32]) {}
            fn f() { take(Maybe::Some(1)); }
        ");
    }

    #[test]
    fn infer_struct_constructor_arg_from_expected() {
        ok("
            struct Box[T](T)
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](1)); }
        ");
    }

    #[test]
    fn infer_class_constructor_arg_from_expected() {
        ok("
            class Box[T](T)
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](1)); }
        ");
    }

    #[test]
    fn infer_enum_none_in_call_arg() {
        ok("
            fn take(value: Option[Int32]) {}
            fn f() { take(None); }
        ");
    }

    #[test]
    fn infer_struct_constructor_none_arg() {
        ok("
            struct Box[T](Option[T])
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](None)); }
        ");
    }

    #[test]
    fn infer_class_constructor_none_arg() {
        ok("
            class Box[T](Option[T])
            fn take(value: Box[Int32]) {}
            fn f() { take(Box[Int32](None)); }
        ");
    }

    #[test]
    fn infer_lambda_call_arg_from_expected() {
        ok("
            fn f(foo: (Int32): Int32): Int32 {
                foo(1)
            }
        ");
    }

    #[test]
    fn infer_lambda_call_none_arg_from_expected() {
        ok("
            fn f(foo: (Option[Int32]): Int32): Int32 {
                foo(None)
            }
        ");
    }

    #[test]
    fn infer_array_index_arg_from_expected() {
        ok("
            fn f(arr: Array[String]): String {
                arr(0)
            }
        ");
    }

    #[test]
    fn call_unknown_function_with_unknown_argument() {
        errors(
            "fn f() { foo(unknown); }",
            vec![
                (
                    (1, 10),
                    3,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_IDENTIFIER,
                    args!("foo"),
                ),
                (
                    (1, 14),
                    7,
                    crate::ErrorLevel::Error,
                    &UNKNOWN_IDENTIFIER,
                    args!("unknown"),
                ),
            ],
        );
    }
}
