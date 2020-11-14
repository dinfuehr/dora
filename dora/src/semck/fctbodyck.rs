use crate::semck::fctbodyck::body::TypeCheck;
use crate::semck::fctbodyck::constck::ConstCheck;
use crate::sym::NestedSymTable;
use crate::vm::{AnalysisData, VM};

use dora_parser::ast::NodeId;

pub mod body;
mod constck;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check(vm: &VM) {
    for fct in vm.fcts.iter() {
        let analysis = {
            let fct = fct.read();

            if !fct.has_body() {
                continue;
            }

            let mut analysis = AnalysisData::new();
            let symtable = NestedSymTable::new(vm, fct.namespace_id);

            let mut typeck = TypeCheck {
                vm,
                fct: &fct,
                file_id: fct.file_id,
                namespace_id: fct.namespace_id,
                analysis: &mut analysis,
                ast: &fct.ast,
                symtable: symtable,
                in_loop: false,
            };

            typeck.check();

            analysis
        };

        fct.write().analysis = Some(analysis);
    }

    for xconst in vm.consts.iter() {
        let mut xconst = xconst.write();

        let (_, value) = {
            let mut constck = ConstCheck {
                vm,
                xconst: &*xconst,
                negative_expr_id: NodeId(0),
            };

            constck.check_expr(&xconst.expr)
        };

        xconst.value = value;
    }
}
