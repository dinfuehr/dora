use dora_bytecode::{BytecodeType, BytecodeTypeArray, ConstPoolEntry, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{Expr, ExprId, TemplateExpr};
use crate::ty::SourceType;

pub(super) fn gen_expr_template(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &TemplateExpr,
    dest: DataDest,
) -> Register {
    let buffer_register = ensure_register(g, dest, BytecodeType::Ptr);

    // build StringBuffer::empty() call
    let fct_id = g.sa.known.functions.string_buffer_empty();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(fct_id));
    g.builder
        .emit_invoke_static(buffer_register, fct_idx, g.loc_for_expr(expr_id));

    let part_register = g.alloc_temp(BytecodeType::Ptr);

    for &part_id in &e.parts {
        let part_expr = g.analysis.expr(part_id);
        if let Expr::LitStr(_) = part_expr {
            let value = g
                .analysis
                .const_value(part_id)
                .to_string()
                .expect("string expected")
                .to_string();
            g.builder.emit_const_string(part_register, value);
        } else {
            let ty = g.ty(part_id);

            if ty.cls_id() == Some(g.sa.known.classes.string()) {
                gen_expr(g, part_id, DataDest::Reg(part_register));
            } else if ty.is_type_param() {
                let type_list_id = match ty {
                    SourceType::TypeParam(id) => id,
                    _ => unreachable!(),
                };

                let expr_register = gen_expr(g, part_id, DataDest::Alloc);
                g.builder.emit_push_register(expr_register);

                // build toString() call
                let name = g.sa.interner.intern("toString");
                let trait_id = g.sa.known.traits.stringable();
                let trait_ = g.sa.trait_(trait_id);
                let to_string_id = trait_
                    .get_method(name, false)
                    .expect("Stringable::toString() not found");

                let fct_idx = g.builder.add_const(ConstPoolEntry::Generic(
                    type_list_id.index() as u32,
                    g.emitter.convert_function_id(to_string_id),
                    BytecodeTypeArray::empty(),
                    BytecodeTypeArray::empty(),
                ));

                g.builder.emit_invoke_generic_direct(
                    part_register,
                    fct_idx,
                    g.loc_for_expr(part_id),
                );

                g.free_if_temp(expr_register);
            } else {
                let expr_register = gen_expr(g, part_id, DataDest::Alloc);
                g.builder.emit_push_register(expr_register);

                // build toString() call
                let (to_string_id, type_params) = g
                    .analysis
                    .get_template(part_id)
                    .expect("missing toString id");

                let type_params = g.convert_tya(&type_params);

                let fct_idx = g
                    .builder
                    .add_const_fct_types(g.emitter.convert_function_id(to_string_id), type_params);
                g.builder
                    .emit_invoke_direct(part_register, fct_idx, g.loc_for_expr(part_id));

                g.free_if_temp(expr_register);
            }
        }

        // build StringBuffer::append() call
        let fct_id = g.sa.known.functions.string_buffer_append();
        let fct_idx = g
            .builder
            .add_const_fct(g.emitter.convert_function_id(fct_id));
        g.builder.emit_push_register(buffer_register);
        g.builder.emit_push_register(part_register);
        let dest_reg = g.ensure_unit_register();
        g.builder
            .emit_invoke_direct(dest_reg, fct_idx, g.loc_for_expr(expr_id));
    }

    g.free_temp(part_register);

    // build StringBuffer::toString() call
    let fct_id = g.sa.known.functions.string_buffer_to_string();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(fct_id));
    g.builder.emit_push_register(buffer_register);
    g.builder
        .emit_invoke_direct(buffer_register, fct_idx, g.loc_for_expr(expr_id));

    buffer_register
}
