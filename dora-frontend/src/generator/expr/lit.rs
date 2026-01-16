use dora_bytecode::{BytecodeType, Register};
use dora_parser::ast;

use super::ensure_register;
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::ExprId;
use crate::ty::SourceType;

pub(super) fn gen_expr_lit_char(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &String,
    _node: ast::AstLitCharExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Char);

    let value = g.analysis.const_value(expr_id).to_char();
    g.builder.emit_const_char(dest, value);

    dest
}

pub(super) fn gen_expr_lit_int(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &String,
    _node: ast::AstLitIntExpr,
    dest: DataDest,
    _neg: bool,
) -> Register {
    let ty = g.analysis.ty(expr_id);
    let value = g.analysis.const_value(expr_id);

    let ty = match ty {
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => {
            let dest = ensure_register(g, dest, BytecodeType::Float32);
            g.builder
                .emit_const_float32(dest, value.to_f64().expect("float expected") as f32);
            return dest;
        }
        SourceType::Float64 => {
            let dest = ensure_register(g, dest, BytecodeType::Float64);
            g.builder
                .emit_const_float64(dest, value.to_f64().expect("float expected"));
            return dest;
        }
        _ => unreachable!(),
    };

    let dest = ensure_register(g, dest, ty.clone());
    let value_i64 = value.to_i64().expect("integer expected");

    match ty {
        BytecodeType::UInt8 => g.builder.emit_const_uint8(dest, value_i64 as u8),
        BytecodeType::Int32 => g.builder.emit_const_int32(dest, value_i64 as i32),
        BytecodeType::Int64 => g.builder.emit_const_int64(dest, value_i64),
        _ => unreachable!(),
    }

    dest
}

pub(super) fn gen_expr_lit_float(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &String,
    _node: ast::AstLitFloatExpr,
    dest: DataDest,
) -> Register {
    let ty = g.analysis.ty(expr_id);
    let value_f64 = g
        .analysis
        .const_value(expr_id)
        .to_f64()
        .expect("float expected");

    let ty = match ty {
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        _ => unreachable!(),
    };

    let dest = ensure_register(g, dest, ty.clone());

    match ty {
        BytecodeType::Float32 => g.builder.emit_const_float32(dest, value_f64 as f32),
        BytecodeType::Float64 => g.builder.emit_const_float64(dest, value_f64),
        _ => unreachable!(),
    }

    dest
}

pub(super) fn gen_expr_lit_string(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    _e: &String,
    _node: ast::AstLitStrExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Ptr);
    let value = g
        .analysis
        .const_value(expr_id)
        .to_string()
        .expect("string expected")
        .to_string();
    g.builder.emit_const_string(dest, value);

    dest
}

pub(super) fn gen_expr_lit_bool(
    g: &mut AstBytecodeGen,
    _expr_id: ExprId,
    e: bool,
    _node: ast::AstLitBoolExpr,
    dest: DataDest,
) -> Register {
    let dest = ensure_register(g, dest, BytecodeType::Bool);

    if e {
        g.builder.emit_const_true(dest);
    } else {
        g.builder.emit_const_false(dest);
    }

    dest
}
