use dora_parser::ast;

use crate::sema::{Sema, lower_expr};

pub fn lower_function_bodies(sa: &mut Sema) {
    let fct_ids: Vec<_> = sa.fcts.iter().map(|(id, _)| id).collect();

    for fct_id in fct_ids {
        let (file_id, block) = {
            let fct = sa.fct(fct_id);
            if !fct.has_body(sa) {
                continue;
            }
            let ast = fct.ast(sa);
            (fct.file_id, ast.block())
        };

        if let Some(block) = block {
            let expr_id = lower_expr(sa, file_id, ast::AstExpr::Block(block));
            let fct = sa.fct(fct_id);
            fct.set_body_expr_id(expr_id);
        }
    }
}
