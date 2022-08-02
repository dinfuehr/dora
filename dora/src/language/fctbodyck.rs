use crate::language::fctbodyck::body::{TypeCheck, VarManager};
use crate::language::fctbodyck::constck::ConstCheck;
use crate::language::sem_analysis::{AnalysisData, FctDefinitionId, SemAnalysis};
use crate::language::sym::ModuleSymTable;

pub mod body;
mod constck;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check(sa: &mut SemAnalysis) {
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
}

fn check_function(sa: &mut SemAnalysis, id: FctDefinitionId) {
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

        let mut typeck = TypeCheck {
            sa,
            fct: &fct,
            package_id: fct.package_id,
            module_id: fct.module_id,
            file_id: fct.file_id,
            analysis: &mut analysis,
            ast: &fct.ast,
            symtable: &mut symtable,
            in_loop: false,
            self_available: false,
            vars: &mut vars,
            contains_lambda: false,
            outer_context_access_in_function: false,
            outer_context_access_from_lambda: false,
        };

        typeck.check();

        analysis
    };

    fct.write().analysis = Some(analysis);
}
