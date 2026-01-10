use dora_parser::ast;

use crate::sema::{Body, ExprArenaBuilder, Sema, lower_expr};

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
            let mut arena = ExprArenaBuilder::new();
            let expr_id = lower_expr(sa, &mut arena, file_id, ast::AstExpr::Block(block));
            let mut body = Body::new_with_arena(arena.freeze());
            body.set_root_expr_id(expr_id);
            let fct = sa.fct(fct_id);
            fct.set_body(body);
        }
    }

    let global_ids: Vec<_> = sa.globals.iter().map(|(id, _)| id).collect();

    for global_id in global_ids {
        let (file_id, init_expr) = {
            let global = sa.global(global_id);
            let ast = global.ast(sa);
            (global.file_id, ast.initial_value())
        };

        if let Some(init_expr) = init_expr {
            let mut arena = ExprArenaBuilder::new();
            let expr_id = lower_expr(sa, &mut arena, file_id, init_expr);
            let mut body = Body::new_with_arena(arena.freeze());
            body.set_root_expr_id(expr_id);
            let global = sa.global(global_id);
            global.set_body(body);
        }
    }
}
