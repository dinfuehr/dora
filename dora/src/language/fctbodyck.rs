use crate::language::fctbodyck::body::TypeCheck;
use crate::language::fctbodyck::constck::ConstCheck;
use crate::language::sym::NestedSymTable;
use crate::vm::{AnalysisData, SemAnalysis};

use dora_parser::ast::NodeId;

pub mod body;
mod constck;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check(sa: &SemAnalysis) {
    for fct in sa.fcts.iter() {
        let analysis = {
            let fct = fct.read();

            if !fct.has_body() {
                continue;
            }

            if fct.is_lambda() {
                // Lambdas will be type-checked by their parent.
                continue;
            }

            let mut analysis = AnalysisData::new();
            let symtable = NestedSymTable::new(sa, fct.namespace_id);

            let mut typeck = TypeCheck {
                sa,
                fct: &fct,
                file_id: fct.file_id,
                namespace_id: fct.namespace_id,
                analysis: &mut analysis,
                ast: &fct.ast,
                symtable: symtable,
                in_loop: false,
                self_ty: None,
            };

            typeck.check();

            analysis
        };

        fct.write().analysis = Some(analysis);
    }

    for xconst in sa.consts.iter() {
        let mut xconst = xconst.write();

        let (_, value) = {
            let mut constck = ConstCheck {
                sa,
                xconst: &*xconst,
                negative_expr_id: NodeId(0),
            };

            constck.check_expr(&xconst.expr)
        };

        xconst.value = value;
    }
}
