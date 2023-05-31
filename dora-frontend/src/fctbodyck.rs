use crate::fctbodyck::body::{TypeCheck, VarManager};
use crate::fctbodyck::constck::ConstCheck;
use crate::sema::{AnalysisData, FctDefinitionId, GlobalDefinitionId, Sema, TypeParamDefinition};
use crate::sym::ModuleSymTable;

pub mod body;
mod constck;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check(sa: &mut Sema) {
    let mut idx = 0;

    while idx < sa.fcts.len() {
        check_function(sa, FctDefinitionId(idx));
        idx += 1;
    }

    for const_ in sa.consts.iter() {
        let mut const_ = const_.write();

        let (_, value) = {
            let mut constck = ConstCheck {
                sa,
                const_: &*const_,
            };

            constck.check_expr(&const_.expr)
        };

        const_.value = value;
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
            fct: Some(&*fct),
            type_param_defs: &fct.type_params,
            package_id: fct.package_id,
            module_id: fct.module_id,
            file_id: fct.file_id,
            analysis: &mut analysis,
            symtable: &mut symtable,
            in_loop: false,
            is_self_available: fct.has_hidden_self_argument(),
            is_lambda: false,
            vars: &mut vars,
            contains_lambda: false,
            outer_context_classes: &mut outer_context_classes,
            outer_context_access_in_function: false,
            outer_context_access_from_lambda: false,
        };

        typeck.check_fct(&fct.ast);

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
            fct: None,
            type_param_defs: &TypeParamDefinition::new(),
            package_id: global.package_id,
            module_id: global.module_id,
            file_id: global.file_id,
            analysis: &mut analysis,
            symtable: &mut symtable,
            in_loop: false,
            is_lambda: false,
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
