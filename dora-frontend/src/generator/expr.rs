use dora_bytecode::{BytecodeType, EnumId, Label, Register};
use dora_parser::ast;

use crate::generator::{bty_array_from_ty, register_bty_from_ty, AstBytecodeGen, DataDest};
use crate::sema::{EnumDefinitionId, IdentType};
use crate::ty::SourceType;

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: &ast::ExprData, dest: DataDest) -> Register {
    match *expr {
        ast::ExprData::Un(ref un) => g.visit_expr_un(un, dest),
        ast::ExprData::Bin(ref bin) => g.visit_expr_bin(bin, dest),
        ast::ExprData::Dot(ref field) => g.visit_expr_dot(field, dest),
        ast::ExprData::Block(ref block) => g.visit_expr_block(block, dest),
        ast::ExprData::If(ref expr) => g.visit_expr_if(expr, dest),
        ast::ExprData::Template(ref template) => g.visit_expr_template(template, dest),
        ast::ExprData::TypeParam(ref expr) => g.visit_expr_type_param(expr, dest),
        ast::ExprData::Path(ref path) => g.visit_expr_path(path, dest),
        ast::ExprData::LitChar(ref lit) => g.visit_expr_lit_char(lit, dest),
        ast::ExprData::LitInt(ref lit) => g.visit_expr_lit_int(lit, dest, false),
        ast::ExprData::LitFloat(ref lit) => g.visit_expr_lit_float(lit, dest),
        ast::ExprData::LitStr(ref lit) => g.visit_expr_lit_string(lit, dest),
        ast::ExprData::LitBool(ref lit) => g.visit_expr_lit_bool(lit, dest),
        ast::ExprData::Ident(ref ident) => g.visit_expr_ident(ident, dest),
        ast::ExprData::Call(ref call) => g.visit_expr_call(call, dest),
        ast::ExprData::This(ref expr) => g.visit_expr_self(expr, dest),
        ast::ExprData::Conv(ref conv) => g.visit_expr_conv(conv, dest),
        ast::ExprData::Tuple(ref tuple) => g.visit_expr_tuple(tuple, dest),
        ast::ExprData::Paren(ref paren) => gen_expr(g, &paren.expr, dest),
        ast::ExprData::Match(ref expr) => gen_match(g, expr, dest),
        ast::ExprData::Lambda(ref node) => g.visit_expr_lambda(node, dest),
        ast::ExprData::For(ref node) => g.visit_expr_for(node, dest),
        ast::ExprData::While(ref node) => g.visit_expr_while(node, dest),
        ast::ExprData::Break(ref node) => g.visit_expr_break(node, dest),
        ast::ExprData::Continue(ref node) => g.visit_expr_continue(node, dest),
        ast::ExprData::Return(ref ret) => g.visit_expr_return(ret, dest),
        ast::ExprData::Error { .. } => unreachable!(),
    }
}

pub(super) fn gen_match(
    g: &mut AstBytecodeGen,
    node: &ast::ExprMatchType,
    dest: DataDest,
) -> Register {
    let result_ty = g.ty(node.id);
    let enum_ty = g.ty(node.expr.id());
    let enum_id = enum_ty.enum_id().expect("enum expected");

    let dest = if result_ty.is_unit() {
        None
    } else {
        let result_bc_ty = register_bty_from_ty(result_ty);
        let dest = g.ensure_register(dest, result_bc_ty);
        Some(dest)
    };

    let end_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, &node.expr, DataDest::Alloc);

    let variant_reg = g.alloc_temp(BytecodeType::Int32);
    let idx = g
        .builder
        .add_const_enum(EnumId(enum_id.0), bty_array_from_ty(&enum_ty.type_params()));
    g.builder
        .emit_load_enum_variant(variant_reg, expr_reg, idx, g.loc(node.span));

    let mut labels = Vec::with_capacity(node.cases.len());

    for case in &node.cases {
        for _pattern in &case.patterns {
            labels.push(g.builder.create_label());
        }
    }

    let mut idx = 0;

    for case in &node.cases {
        for pattern in &case.patterns {
            match pattern.data {
                ast::MatchPatternData::Underscore => {
                    g.builder.emit_jump(labels[idx]);
                }

                ast::MatchPatternData::Ident(_) => {
                    match_check_ident(g, pattern, variant_reg, labels[idx]);
                }
            }

            idx += 1;
        }
    }

    g.builder.emit_jump(end_lbl);
    idx = 0;

    for case in &node.cases {
        for pattern in &case.patterns {
            g.builder.bind_label(labels[idx]);

            match_case_body(
                g,
                case,
                pattern,
                enum_id,
                enum_ty.clone(),
                expr_reg,
                dest,
                end_lbl,
            );

            idx += 1;
        }
    }

    g.builder.bind_label(end_lbl);
    g.free_temp(variant_reg);
    g.free_if_temp(expr_reg);

    dest.unwrap_or(Register::invalid())
}

fn match_check_ident(
    g: &mut AstBytecodeGen,
    pattern: &ast::MatchPattern,
    variant_reg: Register,
    code_lbl: Label,
) {
    let variant_idx = match_variant_idx(g, pattern);

    let tmp_reg = g.alloc_temp(BytecodeType::Int32);
    let cmp_reg = g.alloc_temp(BytecodeType::Bool);
    g.builder.emit_const_int32(tmp_reg, variant_idx as i32);
    g.builder.emit_test_eq(cmp_reg, variant_reg, tmp_reg);
    g.builder.emit_jump_if_true(cmp_reg, code_lbl);
    g.free_temp(tmp_reg);
    g.free_temp(cmp_reg);
}

fn match_case_body(
    g: &mut AstBytecodeGen,
    case: &ast::MatchCaseType,
    pattern: &ast::MatchPattern,
    enum_id: EnumDefinitionId,
    enum_ty: SourceType,
    expr_reg: Register,
    dest: Option<Register>,
    end_lbl: Label,
) {
    g.push_scope();

    if let ast::MatchPatternData::Ident(ref ident) = pattern.data {
        if let Some(ref params) = ident.params {
            let variant_idx = match_variant_idx(g, pattern);

            for (subtype_idx, param) in params.iter().enumerate() {
                if let Some(_) = param.name {
                    let idx = g.builder.add_const_enum_element(
                        EnumId(enum_id.0),
                        bty_array_from_ty(&enum_ty.type_params()),
                        variant_idx,
                        subtype_idx as u32,
                    );

                    let var_id = *g.analysis.map_vars.get(param.id).unwrap();

                    let ty = g.var_ty(var_id);

                    if !ty.is_unit() {
                        let ty: BytecodeType = register_bty_from_ty(ty);
                        let var_reg = g.alloc_var(ty);

                        g.var_registers.insert(var_id, var_reg);

                        g.builder
                            .emit_load_enum_element(var_reg, expr_reg, idx, g.loc(param.span));
                    }
                }
            }
        }
    }

    if let Some(dest) = dest {
        gen_expr(g, &case.value, DataDest::Reg(dest));
    } else {
        gen_expr(g, &case.value, DataDest::Effect);
    }

    g.builder.emit_jump(end_lbl);
    g.pop_scope();
}

fn match_variant_idx(g: &AstBytecodeGen, pattern: &ast::MatchPattern) -> u32 {
    let ident_type = g.analysis.map_idents.get(pattern.id).unwrap();

    match ident_type {
        IdentType::EnumValue(_, _, variant_idx) => (*variant_idx).try_into().unwrap(),
        _ => unreachable!(),
    }
}
