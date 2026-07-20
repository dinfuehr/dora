use crate::SourceType;
use crate::SourceTypeArray;
use crate::element_collector::Annotations;
use crate::sema::{Body, ExprId, FctDefinition, FctParent, LambdaExpr, LambdaId, Param, Params};
use crate::typeck::TypeCheck;
use crate::{args, error::diagnostics::VARIADIC_PARAMETER_NEEDS_TO_BE_LAST};

pub(super) fn check_expr_lambda(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &LambdaExpr,
    expected_ty: SourceType,
) -> SourceType {
    // Extract expected param and return types from expected_ty if it's a lambda
    let (expected_params, expected_return_type) = match expected_ty.to_lambda() {
        Some((params, ret, _)) => (Some(params), Some(ret)),
        None => (None, None),
    };

    for lambda_param in sema_expr.params.iter().rev().skip(1) {
        if lambda_param.variadic {
            ck.report(
                sema_expr.span,
                &VARIADIC_PARAMETER_NEEDS_TO_BE_LAST,
                args!(),
            );
        }
    }
    let is_variadic = sema_expr.params.last().is_some_and(|param| param.variadic);

    let lambda_return_type = if let Some(ret_ty) = sema_expr.return_ty {
        // Explicit annotation takes precedence
        ck.read_type(ret_ty)
    } else if let Some(expected_ret) = expected_return_type {
        // Use expected return type from context
        expected_ret
    } else {
        // Default to Unit
        SourceType::Unit
    };

    // Check for parameter count mismatch
    let param_count_mismatch = if let Some(ref expected) = expected_params {
        let actual_count = sema_expr.params.len();
        let expected_count = expected.types().len();

        if actual_count != expected_count {
            ck.report(
                sema_expr.span,
                &crate::error::diagnostics::LAMBDA_PARAM_COUNT_MISMATCH,
                crate::args!(actual_count.to_string(), expected_count.to_string()),
            );
        }

        actual_count != expected_count
    } else {
        false
    };

    let mut params = Vec::new();

    for (idx, lambda_param) in sema_expr.params.iter().enumerate() {
        let ty = if let Some(ty_id) = lambda_param.ty {
            // Explicit annotation takes precedence
            ck.read_type(ty_id)
        } else if let Some(ref expected) = expected_params {
            if param_count_mismatch {
                SourceType::Error
            } else {
                expected.types().get(idx).cloned().expect("missing index")
            }
        } else {
            // No annotation and no expected type - report error
            ck.report(
                sema_expr.span,
                &crate::error::diagnostics::LAMBDA_PARAM_MISSING_TYPE,
                crate::args!(),
            );
            SourceType::Error
        };
        let param = Param::new_ty(ty.clone());
        params.push(param);
    }

    let param_types = params.iter().map(|p| p.ty()).collect::<Vec<_>>();
    let ty = SourceType::Lambda(
        SourceTypeArray::with(param_types),
        Box::new(lambda_return_type.clone()),
        is_variadic,
    );

    let param = Param::new_ty(SourceType::Ptr);
    let mut lambda_params = vec![param];
    lambda_params.append(&mut params);

    // Collect lambda parameter pattern IDs
    let param_pattern_ids: Vec<_> = sema_expr.params.iter().map(|p| p.pattern).collect();

    let body = {
        let mut body = Body::new_with_arenas(
            ck.body.arena(),
            ck.body.stmt_arena(),
            ck.body.pattern_arena(),
            ck.body.type_ref_arena(),
        );
        body.set_param_pattern_ids(param_pattern_ids);
        body.set_root_expr_id(sema_expr.block);

        let mut typeck = TypeCheck {
            sa: ck.sa,
            type_param_definition_id: ck.type_param_definition_id,
            type_param_definition: ck.type_param_definition,
            package_id: ck.package_id,
            module_id: ck.module_id,
            file_id: ck.file_id,
            body: &body,
            symtable: &mut ck.symtable,
            in_loop: false,
            is_lambda: true,
            param_types: lambda_params.clone(),
            is_variadic,
            return_type: Some(lambda_return_type.clone()),
            parent: ck.parent.clone(),
            is_in_trait: ck.is_in_trait,
            has_hidden_self_argument: true,
            is_self_available: ck.is_self_available,
            is_mutating: ck.is_mutating,
            self_ty: ck.self_ty.clone(),
            vars: ck.vars,
            lambda_definitions: ck.lambda_definitions,
            contexts: ck.contexts,
            active_contexts: ck.active_contexts,
            start_context_idx: 0,
            needs_context_slot_in_lambda_object: false,
            type_variables: ck.type_variables,
            element: ck.element,
        };

        typeck.check_lambda(sema_expr);

        body
    };

    let name = ck.sa.generate_lambda_name();
    let name = ck.sa.interner.intern(&name);

    let lambda = FctDefinition::new_no_source(
        ck.package_id,
        ck.module_id,
        ck.file_id,
        sema_expr.declaration_span,
        sema_expr.span,
        Some(ck.body.exprs().syntax_node_ptr(expr_id)),
        Annotations::default(),
        name,
        ck.type_param_definition_id,
        Params::new(lambda_params, true, is_variadic),
        lambda_return_type.clone(),
        FctParent::Function,
        ck.is_in_trait,
    );
    lambda
        .parsed_return_type()
        .set_ty(lambda_return_type.clone());
    lambda.set_body(body);

    let lambda_id = LambdaId(ck.lambda_definitions.len());
    ck.lambda_definitions.push(lambda);
    ck.body.insert_lambda_id(expr_id, lambda_id);
    ck.body.set_ty(expr_id, ty.clone());

    if param_count_mismatch {
        SourceType::Error
    } else {
        ty
    }
}
