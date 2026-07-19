use dora_bytecode::{BytecodeTypeArray, ConstPoolEntry, Register};

use super::{ensure_register, gen_expr};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{Expr, ExprId, TemplateExpr};
use crate::ty::{SourceType, SourceTypeArray, TraitType};

pub(super) fn gen_expr_template(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &TemplateExpr,
    dest: DataDest,
) -> Register {
    let string_ty = SourceType::Class(g.sa.known.classes.string(), SourceTypeArray::empty());
    let string_ty = g.emitter.convert_ty(g.sa, string_ty);
    let result_register = ensure_register(g, dest, string_ty.clone());

    let buffer_ty = SourceType::Class(g.sa.known.classes.string_buffer(), SourceTypeArray::empty());
    let buffer_ty = g.emitter.convert_ty(g.sa, buffer_ty);
    let buffer_register = g.alloc_temp(buffer_ty);

    // build StringBuffer::empty() call
    let fct_id = g.sa.known.functions.string_buffer_empty();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(g.sa, fct_id));
    g.builder
        .emit_invoke_static(buffer_register, fct_idx, &[], g.loc_for_expr(expr_id));

    let part_register = g.alloc_temp(string_ty);

    for &part_id in &e.parts {
        let part_expr = g.analysis.expr(part_id);
        if let Expr::LitStr(_) = part_expr {
            let value = g
                .analysis
                .get_const_value(part_id)
                .expect("missing literal")
                .to_string()
                .expect("string expected")
                .to_string();
            g.builder.emit_const_string(part_register, value);
        } else {
            let ty = g.ty(part_id);

            if ty.cls_id() == Some(g.sa.known.classes.string()) {
                gen_expr(g, part_id, DataDest::Reg(part_register));
            } else if ty.is_type_param() || ty.is_assoc() || ty.is_generic_assoc() {
                let expr_register = gen_expr(g, part_id, DataDest::Alloc);

                // build to_string() call
                let name = g.sa.interner.intern("to_string");
                let trait_id = g.sa.known.traits.stringable();
                let trait_ = g.sa.trait_(trait_id);
                let to_string_id = trait_
                    .get_method(name, false)
                    .expect("Stringable::to_string() not found");

                let trait_ty = TraitType {
                    trait_id,
                    type_params: SourceTypeArray::empty(),
                    bindings: Vec::new(),
                };
                let fct_idx = g.builder.add_const(ConstPoolEntry::Generic {
                    object_type: g.emitter.convert_ty(g.sa, ty),
                    trait_ty: g.emitter.convert_trait_ty(g.sa, &trait_ty),
                    fct_id: g.emitter.convert_function_id(g.sa, to_string_id),
                    fct_type_params: BytecodeTypeArray::empty(),
                });

                g.builder.emit_invoke_generic_direct(
                    part_register,
                    fct_idx,
                    &[expr_register],
                    g.loc_for_expr(part_id),
                );

                g.free_if_temp(expr_register);
            } else {
                let expr_register = gen_expr(g, part_id, DataDest::Alloc);

                // build to_string() call
                let (to_string_id, type_params) = g
                    .analysis
                    .get_template(part_id)
                    .expect("missing to_string id");

                let type_params = g.convert_tya(&type_params);

                let fct_idx = g.builder.add_const_fct_types(
                    g.emitter.convert_function_id(g.sa, to_string_id),
                    type_params,
                );
                g.builder.emit_invoke_direct(
                    part_register,
                    fct_idx,
                    &[expr_register],
                    g.loc_for_expr(part_id),
                );

                g.free_if_temp(expr_register);
            }
        }

        // build StringBuffer::append() call
        let fct_id = g.sa.known.functions.string_buffer_append();
        let fct_idx = g
            .builder
            .add_const_fct(g.emitter.convert_function_id(g.sa, fct_id));
        let dest_reg = g.ensure_unit_register();
        g.builder.emit_invoke_direct(
            dest_reg,
            fct_idx,
            &[buffer_register, part_register],
            g.loc_for_expr(expr_id),
        );
    }

    g.free_temp(part_register);

    // build StringBuffer::to_string() call
    let fct_id = g.sa.known.functions.string_buffer_to_string();
    let fct_idx = g
        .builder
        .add_const_fct(g.emitter.convert_function_id(g.sa, fct_id));
    g.builder.emit_invoke_direct(
        result_register,
        fct_idx,
        &[buffer_register],
        g.loc_for_expr(expr_id),
    );
    g.free_temp(buffer_register);

    result_register
}
