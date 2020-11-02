use crate::sym::SymTables;
use crate::typeck::constck::ConstCheck;
use crate::typeck::expr::TypeCheck;
use crate::vm::VM;

use dora_parser::ast::NodeId;

mod constck;
pub mod expr;
mod lookup;
#[cfg(test)]
mod tests;

pub fn check(vm: &VM) {
    for fct in vm.fcts.iter() {
        let fct = fct.read();

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.write();

        let symtable = SymTables::current(vm, fct.namespace_id);

        let mut typeck = TypeCheck {
            vm,
            fct: &fct,
            file: fct.file_id,
            src: &mut src,
            ast: &fct.ast,
            symtable: symtable,
        };

        typeck.check();
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
