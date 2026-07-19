use super::gen_expr;
use crate::generator::pattern::{destruct_pattern, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest, gen_unreachable};
use crate::sema::{ExprId, LetStmt};

pub(in crate::generator) fn gen_stmt_expr(g: &mut AstBytecodeGen, expr_id: ExprId) {
    let reg = gen_expr(g, expr_id, DataDest::Alloc);
    g.free_if_temp(reg);
}

pub(in crate::generator) fn gen_stmt_let(g: &mut AstBytecodeGen, stmt: &LetStmt) {
    setup_pattern_vars(g, stmt.pattern);

    if let Some(expr_id) = stmt.expr {
        let ty = g.ty(expr_id);
        let value = gen_expr(g, expr_id, DataDest::Alloc);
        let mismatch_lbl = destruct_pattern(g, stmt.pattern, value, ty, None);
        g.free_if_temp(value);

        if let Some(mismatch_lbl) = mismatch_lbl {
            let merge_lbl = g.builder.create_label();
            g.builder.emit_jump(merge_lbl);
            g.builder.bind_label(mismatch_lbl);

            if let Some(else_expr) = stmt.else_expr {
                let result = gen_expr(g, else_expr, DataDest::Alloc);
                g.free_if_temp(result);
            } else {
                gen_unreachable(g, g.span_for_pattern(stmt.pattern));
            }

            g.builder.bind_label(merge_lbl);
        }
    }
}
