use crate::sema::{AnalysisData, FctDefinitionId, GlobalDefinitionId, Sema, TypeParamDefinition};
use crate::sym::ModuleSymTable;
use crate::typeck::call::{check_expr_call, check_expr_call_enum_args, find_method, lookup_method};
use crate::typeck::constck::ConstCheck;
use crate::typeck::control::{
    check_expr_break_and_continue, check_expr_for, check_expr_if, check_expr_return,
    check_expr_while,
};
use crate::typeck::expr::{check_enum_value_with_args, check_expr, read_path_expr};
use crate::typeck::function::{
    args_compatible, args_compatible_fct, check_lit_char, check_lit_float, check_lit_int,
    check_lit_str, is_simple_enum, TypeCheck, VarManager,
};
use crate::typeck::lookup::MethodLookup;
use crate::typeck::stmt::{check_let_pattern, check_stmt};

mod call;
mod constck;
mod control;
mod expr;
pub mod function;
mod lookup;
mod stmt;
#[cfg(test)]
mod tests;

pub fn check(sa: &mut Sema) {
    let mut idx = 0;

    while idx < sa.fcts.len() {
        check_function(sa, FctDefinitionId(idx));
        idx += 1;
    }

    for (_const_id, const_) in sa.consts.iter() {
        let (_, value) = {
            let mut constck = ConstCheck {
                sa,
                const_: &*const_,
            };

            constck.check_expr(&const_.expr)
        };

        const_.value.set(value).expect("already initialized");
    }

    let mut idx: u32 = 0;

    while idx < sa.globals.len() as u32 {
        check_global(sa, GlobalDefinitionId(idx));
        idx += 1;
    }
}

fn check_function(sa: &mut Sema, id: FctDefinitionId) {
    let fct = sa.fcts.idx(id);

    let analysis = {
        let fct = fct.read();

        if !fct.has_body() {
            return;
        }

        if fct.is_lambda() {
            // Lambdas will be type-checked by their parent.
            return;
        }

        let mut analysis = AnalysisData::new();
        let mut symtable = ModuleSymTable::new(sa, fct.module_id);
        let mut vars = VarManager::new();
        let mut outer_context_classes = Vec::new();

        let mut typeck = TypeCheck {
            sa,
            type_param_defs: &fct.type_params,
            package_id: fct.package_id,
            module_id: fct.module_id,
            file_id: fct.file_id,
            analysis: &mut analysis,
            symtable: &mut symtable,
            param_types: fct.params_with_self().to_owned(),
            return_type: Some(fct.return_type.clone()),
            in_loop: false,
            has_hidden_self_argument: fct.has_hidden_self_argument(),
            is_self_available: fct.has_hidden_self_argument(),
            is_lambda: false,
            vars: &mut vars,
            contains_lambda: false,
            outer_context_classes: &mut outer_context_classes,
            outer_context_access_in_function: false,
            outer_context_access_from_lambda: false,
        };

        typeck.check_fct(&*fct, &fct.ast);

        analysis
    };

    fct.write().analysis = Some(analysis);
}

fn check_global(sa: &mut Sema, id: GlobalDefinitionId) {
    let global = sa.globals.idx(id);

    let analysis = {
        let global = global.read();

        if global.ast.initial_value.is_none() {
            return;
        }

        let mut analysis = AnalysisData::new();
        let mut symtable = ModuleSymTable::new(sa, global.module_id);
        let mut vars = VarManager::new();
        let mut outer_context_classes = Vec::new();

        let mut typeck = TypeCheck {
            sa,
            type_param_defs: &TypeParamDefinition::new(),
            package_id: global.package_id,
            module_id: global.module_id,
            file_id: global.file_id,
            analysis: &mut analysis,
            symtable: &mut symtable,
            in_loop: false,
            is_lambda: false,
            param_types: Vec::new(),
            return_type: None,
            has_hidden_self_argument: false,
            is_self_available: false,
            vars: &mut vars,
            contains_lambda: false,
            outer_context_classes: &mut outer_context_classes,
            outer_context_access_in_function: false,
            outer_context_access_from_lambda: false,
        };

        typeck.check_initializer(
            &*global,
            global.ast.initial_value.as_ref().expect("missing expr"),
        );

        analysis
    };

    global.write().analysis = Some(analysis);
}
