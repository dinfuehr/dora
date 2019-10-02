use std::collections::hash_set::HashSet;

use crate::ty::BuiltinType;
use crate::typeck::constck::ConstCheck;
use crate::typeck::expr::TypeCheck;
use crate::vm::VM;

use dora_parser::ast::NodeId;

mod constck;
mod expr;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check<'a, 'ast>(vm: &VM<'ast>) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();
        let ast = fct.ast;

        let mut typeck = TypeCheck {
            vm: vm,
            fct: &fct,
            file: fct.file,
            src: &mut src,
            ast: ast,
            expr_type: BuiltinType::Unit,
            negative_expr_id: NodeId(0),
            used_in_call: HashSet::new(),
        };

        typeck.check();
    }

    for xconst in vm.consts.iter() {
        let mut xconst = xconst.lock();

        let (_, value) = {
            let mut constck = ConstCheck {
                vm: vm,
                xconst: &*xconst,
                negative_expr_id: NodeId(0),
            };

            constck.check_expr(&xconst.expr)
        };

        xconst.value = value;
    }
}
