use dora_parser::ast;

use crate::sema::{
    AnalysisData, Element, FctDefinition, FctParent, GlobalDefinition,
    LazyContextClassCreationData, LazyLambdaCreationData, Sema,
};
use crate::sym::ModuleSymTable;
use crate::typeck::call::{check_expr_call, create_call_arguments};
use crate::typeck::constck::ConstCheck;
use crate::typeck::control::{
    check_expr_break_and_continue, check_expr_for, check_expr_if, check_expr_match,
    check_expr_return, check_expr_while, get_subpatterns,
};
use crate::typeck::expr::{check_expr, read_path, read_path_expr};
pub use crate::typeck::expr::{compute_lit_float, compute_lit_int};
use crate::typeck::function::{
    add_local, check_args_compatible, check_args_compatible_fct, check_args_compatible_fct2,
    check_lit_char, check_lit_float, check_lit_int, check_lit_str, is_simple_enum, TypeCheck,
    VarManager,
};
use crate::typeck::lookup::find_method_call_candidates;
use crate::typeck::stmt::{check_pattern, check_stmt};
use crate::typeck::type_params::check_type_params;
use crate::{SourceType, Span};

mod call;
mod constck;
mod control;
mod expr;
pub mod function;
mod lookup;
mod stmt;
#[cfg(test)]
mod tests;
mod type_params;

pub fn check(sa: &mut Sema) {
    let mut lazy_context_class_creation = Vec::new();
    let mut lazy_lambda_creation = Vec::new();

    for (_id, fct) in sa.fcts.iter() {
        if fct.has_body(sa) {
            check_function(
                sa,
                fct,
                &mut lazy_context_class_creation,
                &mut lazy_lambda_creation,
            );
        }
    }

    for (_const_id, const_) in sa.consts.iter() {
        let (_, value) = {
            let mut constck = ConstCheck {
                sa,
                const_: &*const_,
            };

            constck.check_expr(const_.expr)
        };

        const_.value.set(value).expect("already initialized");
    }

    for (_id, global) in sa.globals.iter() {
        check_global(
            sa,
            global,
            &mut lazy_context_class_creation,
            &mut lazy_lambda_creation,
        );
    }

    create_context_classes(sa, lazy_context_class_creation);
    create_lambda_functions(sa, lazy_lambda_creation);
}

fn check_function(
    sa: &Sema,
    fct: &FctDefinition,
    lazy_context_class_creation: &mut Vec<LazyContextClassCreationData>,
    lazy_lambda_creation: &mut Vec<LazyLambdaCreationData>,
) {
    let mut analysis = AnalysisData::new();
    let mut symtable = ModuleSymTable::new(sa, fct.module_id);
    let mut vars = VarManager::new();
    let mut context_classes = Vec::new();

    let self_ty = match fct.parent {
        FctParent::None => None,
        FctParent::Extension(id) => Some(sa.extension(id).ty().clone()),
        FctParent::Impl(id) => Some(sa.impl_(id).extended_ty()),
        FctParent::Trait(..) => Some(SourceType::This),
        FctParent::Function => unreachable!(),
    };

    let mut typeck = TypeCheck {
        sa,
        type_param_definition: fct.type_param_definition(),
        package_id: fct.package_id,
        module_id: fct.module_id,
        file_id: fct.file_id,
        analysis: &mut analysis,
        symtable: &mut symtable,
        param_types: fct.params_with_self().to_owned(),
        return_type: Some(fct.return_type()),
        in_loop: false,
        parent: fct.parent.clone(),
        has_hidden_self_argument: fct.has_hidden_self_argument(),
        is_self_available: fct.has_hidden_self_argument(),
        self_ty,
        is_lambda: false,
        vars: &mut vars,
        lazy_context_class_creation,
        lazy_lambda_creation,
        context_classes: &mut context_classes,
        start_context_id: 0,
        needs_context_slot_in_lambda_object: false,
        element: fct,
    };

    typeck.check_fct(fct.ast(sa));

    assert!(fct.analysis.set(analysis).is_ok());
}

fn check_global(
    sa: &Sema,
    global: &GlobalDefinition,
    lazy_context_class_creation: &mut Vec<LazyContextClassCreationData>,
    lazy_lambda_creation: &mut Vec<LazyLambdaCreationData>,
) {
    let analysis = {
        if !global.has_initial_value() {
            return;
        }

        let mut analysis = AnalysisData::new();
        let mut symtable = ModuleSymTable::new(sa, global.module_id);
        let mut vars = VarManager::new();
        let mut outer_context_classes = Vec::new();

        let mut typeck = TypeCheck {
            sa,
            type_param_definition: global.type_param_definition(),
            package_id: global.package_id,
            module_id: global.module_id,
            file_id: global.file_id,
            analysis: &mut analysis,
            symtable: &mut symtable,
            in_loop: false,
            is_lambda: false,
            param_types: Vec::new(),
            return_type: None,
            parent: FctParent::None,
            has_hidden_self_argument: false,
            is_self_available: false,
            self_ty: None,
            vars: &mut vars,
            lazy_context_class_creation,
            lazy_lambda_creation,
            context_classes: &mut outer_context_classes,
            start_context_id: 0,
            needs_context_slot_in_lambda_object: false,
            element: global,
        };

        typeck.check_initializer(
            &*global,
            global.initial_value_ast_id.expect("missing initializer"),
        );

        analysis
    };
    assert!(global.analysis.set(analysis).is_ok());
}

fn create_context_classes(sa: &mut Sema, lazy_classes: Vec<LazyContextClassCreationData>) {
    for lazy_class in lazy_classes {
        let class_id = sa.classes.alloc(lazy_class.class_definition);
        sa.classes[class_id].id = Some(class_id);

        let field_ids = lazy_class
            .fields
            .into_iter()
            .map(|field| sa.fields.alloc(field))
            .collect::<Vec<_>>();
        assert!(sa.class(class_id).field_ids.set(field_ids).is_ok());

        lazy_class.context.set_class_id(class_id);
    }
}

fn create_lambda_functions(sa: &mut Sema, lazy_lambdas: Vec<LazyLambdaCreationData>) {
    for lazy_lambda in lazy_lambdas {
        let fct_id = sa.fcts.alloc(lazy_lambda.fct_definition);
        sa.fcts[fct_id].id = Some(fct_id);
        lazy_lambda.id.set_fct_id(fct_id);
    }
}

pub struct CallArguments {
    arguments: Vec<ast::AstId>,
    span: Span,
}
