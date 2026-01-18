use std::rc::Rc;

use dora_parser::ast;

use crate::sema::{
    Body, ExprArenaBuilder, PatternArenaBuilder, Sema, StmtArenaBuilder, TypeRefArenaBuilder,
    lower_expr, lower_pattern_opt,
};

pub fn lower_bodies(sa: &mut Sema) {
    lower_function_bodies(sa);
    lower_global_bodies(sa);
    lower_const_bodies(sa);
}

fn lower_function_bodies(sa: &mut Sema) {
    let fct_ids: Vec<_> = sa.fcts.iter().map(|(id, _)| id).collect();

    for fct_id in fct_ids {
        let (file_id, ast_fct, block) = {
            let fct = sa.fct(fct_id);
            if !fct.has_body(sa) {
                continue;
            }
            let ast = fct.ast(sa);
            (fct.file_id, ast.clone(), ast.block())
        };

        if let Some(block) = block {
            let mut expr_arena = ExprArenaBuilder::new();
            let mut stmt_arena = StmtArenaBuilder::new();
            let mut pattern_arena = PatternArenaBuilder::new();
            let mut type_ref_arena = TypeRefArenaBuilder::new();

            // Lower function parameter patterns into the arena
            let param_pattern_ids: Vec<_> = ast_fct
                .params()
                .map(|param| lower_pattern_opt(sa, &mut pattern_arena, file_id, param.pattern()))
                .collect();

            let expr_id = lower_expr(
                sa,
                &mut expr_arena,
                &mut stmt_arena,
                &mut pattern_arena,
                &mut type_ref_arena,
                file_id,
                ast::AstExpr::BlockExpr(block),
            );
            let mut body = Body::new_with_arenas(
                expr_arena.freeze(),
                stmt_arena.freeze(),
                pattern_arena.freeze(),
                Rc::new(type_ref_arena.freeze()),
            );
            body.set_root_expr_id(expr_id);
            body.set_param_pattern_ids(param_pattern_ids);
            let fct = sa.fct(fct_id);
            fct.set_body(body);
        }
    }
}

fn lower_global_bodies(sa: &mut Sema) {
    let global_ids: Vec<_> = sa.globals.iter().map(|(id, _)| id).collect();

    for global_id in global_ids {
        let (file_id, init_expr) = {
            let global = sa.global(global_id);
            let ast = global.ast(sa);
            (global.file_id, ast.initial_value())
        };

        if let Some(init_expr) = init_expr {
            let mut expr_arena = ExprArenaBuilder::new();
            let mut stmt_arena = StmtArenaBuilder::new();
            let mut pattern_arena = PatternArenaBuilder::new();
            let mut type_ref_arena = TypeRefArenaBuilder::new();
            let expr_id = lower_expr(
                sa,
                &mut expr_arena,
                &mut stmt_arena,
                &mut pattern_arena,
                &mut type_ref_arena,
                file_id,
                init_expr,
            );
            let mut body = Body::new_with_arenas(
                expr_arena.freeze(),
                stmt_arena.freeze(),
                pattern_arena.freeze(),
                Rc::new(type_ref_arena.freeze()),
            );
            body.set_root_expr_id(expr_id);
            body.set_param_pattern_ids(Vec::new());
            let global = sa.global(global_id);
            global.set_body(body);
        }
    }
}

fn lower_const_bodies(sa: &mut Sema) {
    let const_ids: Vec<_> = sa.consts.iter().map(|(id, _)| id).collect();

    for const_id in const_ids {
        let (file_id, expr) = {
            let const_ = sa.const_(const_id);
            let ast = const_.ast(sa);
            (const_.file_id, ast.expr())
        };

        if let Some(expr) = expr {
            let mut expr_arena = ExprArenaBuilder::new();
            let mut stmt_arena = StmtArenaBuilder::new();
            let mut pattern_arena = PatternArenaBuilder::new();
            let mut type_ref_arena = TypeRefArenaBuilder::new();
            let expr_id = lower_expr(
                sa,
                &mut expr_arena,
                &mut stmt_arena,
                &mut pattern_arena,
                &mut type_ref_arena,
                file_id,
                expr,
            );
            let mut body = Body::new_with_arenas(
                expr_arena.freeze(),
                stmt_arena.freeze(),
                pattern_arena.freeze(),
                Rc::new(type_ref_arena.freeze()),
            );
            body.set_root_expr_id(expr_id);
            body.set_param_pattern_ids(Vec::new());
            let const_ = sa.const_(const_id);
            const_.set_body(body);
        }
    }
}
