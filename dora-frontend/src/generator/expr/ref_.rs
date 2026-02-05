use dora_bytecode::Register;

use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, RefExpr};

pub(super) fn gen_expr_ref(
    _g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    _e: &RefExpr,
    _dest: DataDest,
) -> Register {
    unimplemented!()
}
