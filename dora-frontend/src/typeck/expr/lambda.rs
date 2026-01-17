use dora_parser::ast::{self, SyntaxNodeBase};

use crate::SourceType;
use crate::SourceTypeArray;
use crate::element_collector::Annotations;
use crate::sema::{
    Body, ExprId, FctDefinition, FctParent, LambdaExpr, LazyLambdaCreationData, LazyLambdaId,
    Param, Params,
};
use crate::typeck::TypeCheck;

pub(super) fn check_expr_lambda(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &LambdaExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let lambda_return_type = if let Some(ret_ty) = sema_expr.return_ty {
        ck.read_type_id(ret_ty)
    } else {
        SourceType::Unit
    };

    let mut params = Vec::new();

    for lambda_param in &sema_expr.params {
        let ty = if let Some(ty_id) = lambda_param.ty {
            ck.read_type_id(ty_id)
        } else {
            SourceType::Error
        };
        let param = Param::new_ty(ty.clone());
        params.push(param);
    }

    let param_types = params.iter().map(|p| p.ty()).collect::<Vec<_>>();
    let ty = SourceType::Lambda(
        SourceTypeArray::with(param_types),
        Box::new(lambda_return_type.clone()),
    );

    let param = Param::new_ty(SourceType::Ptr);
    let mut lambda_params = vec![param];
    lambda_params.append(&mut params);

    // Collect lambda parameter pattern IDs
    let param_pattern_ids: Vec<_> = sema_expr.params.iter().map(|p| p.pattern).collect();

    let body = {
        let body = Body::new_with_arenas(
            ck.body.arena(),
            ck.body.stmt_arena(),
            ck.body.pattern_arena(),
        );
        body.set_outer_contexts(ck.context_classes.clone());
        body.set_param_pattern_ids(param_pattern_ids);

        let mut typeck = TypeCheck {
            sa: ck.sa,
            type_param_definition: ck.type_param_definition,
            package_id: ck.package_id,
            module_id: ck.module_id,
            file_id: ck.file_id,
            body: &body,
            symtable: &mut ck.symtable,
            in_loop: false,
            is_lambda: true,
            param_types: lambda_params.clone(),
            return_type: Some(lambda_return_type.clone()),
            parent: ck.parent.clone(),
            has_hidden_self_argument: true,
            is_self_available: ck.is_self_available,
            self_ty: ck.self_ty.clone(),
            vars: ck.vars,
            lazy_context_class_creation: ck.lazy_context_class_creation,
            lazy_lambda_creation: ck.lazy_lambda_creation,
            context_classes: ck.context_classes,
            start_context_id: 0,
            needs_context_slot_in_lambda_object: false,
            element: ck.element,
        };

        // Load AST node for check_lambda (needs AST for block processing)
        let node = typeck.syntax_by_id::<ast::AstLambdaExpr>(expr_id);
        typeck.check_lambda(node);

        body
    };

    let name = ck.sa.generate_lambda_name();
    let name = ck.sa.interner.intern(&name);

    // Load AST for spans and source
    let node = ck.syntax_by_id::<ast::AstLambdaExpr>(expr_id);

    let lambda = FctDefinition::new_no_source(
        ck.package_id,
        ck.module_id,
        ck.file_id,
        node.declaration_span(),
        node.span(),
        Some(node.into()),
        Annotations::default(),
        name,
        ck.type_param_definition.clone(),
        Params::new(lambda_params, true, false),
        lambda_return_type.clone(),
        FctParent::Function,
    );
    lambda
        .parsed_return_type()
        .set_ty(lambda_return_type.clone());
    lambda.set_body(body);

    let lambda_id = LazyLambdaId::new();

    ck.lazy_lambda_creation.push(LazyLambdaCreationData {
        id: lambda_id.clone(),
        fct_definition: lambda,
    });
    ck.body.insert_lambda(expr_id, lambda_id);
    ck.body.set_ty(expr_id, ty.clone());

    ty
}
