use dora_bytecode::{BytecodeType, ConstPoolEntry, ConstPoolIdx, Location, Register};
use dora_parser::ast::AstExpr;

mod as_;
mod assign;
mod bin;
mod block;
mod break_;
mod call;
mod continue_;
mod field;
mod for_;
mod if_;
mod is;
mod lambda;
mod lit;
mod match_;
mod method_call;
mod path;
mod return_;
mod self_;
mod stmt;
mod template;
mod tuple;
mod un;
mod while_;

use self::as_::gen_expr_as;
use self::bin::gen_expr_bin;
use self::block::gen_expr_block;
use self::break_::gen_expr_break;
use self::call::gen_expr_call;
use self::continue_::gen_expr_continue;
use self::field::gen_expr_field;
use self::for_::gen_expr_for;
use self::if_::gen_expr_if;
use self::is::gen_expr_is;
use self::lambda::gen_expr_lambda;
use self::lit::{
    gen_expr_lit_bool, gen_expr_lit_char, gen_expr_lit_float, gen_expr_lit_int, gen_expr_lit_string,
};
use self::match_::gen_match;
use self::method_call::gen_expr_method_call;
use self::path::gen_expr_path;
use self::return_::gen_expr_return;
use self::self_::gen_expr_self;
pub(in crate::generator) use self::stmt::{gen_stmt_expr, gen_stmt_let};
use self::template::gen_expr_template;
use self::tuple::gen_expr_tuple;
use self::un::gen_expr_un;
use self::while_::gen_expr_while;
use super::{AstBytecodeGen, DataDest};
use crate::sema::{CallType, FctDefinition};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::SourceType;

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr: AstExpr, dest: DataDest) -> Register {
    match expr {
        AstExpr::UnExpr(node) => gen_expr_un(g, node, dest),
        AstExpr::AssignExpr(node) => self::assign::gen_expr_assign(g, node, dest),
        AstExpr::BinExpr(node) => gen_expr_bin(g, node, dest),
        AstExpr::FieldExpr(node) => gen_expr_field(g, node, dest),
        AstExpr::BlockExpr(node) => gen_expr_block(g, node, dest),
        AstExpr::IfExpr(node) => gen_expr_if(g, node, dest),
        AstExpr::TemplateExpr(node) => gen_expr_template(g, node, dest),
        AstExpr::LitCharExpr(node) => gen_expr_lit_char(g, node, dest),
        AstExpr::LitIntExpr(node) => gen_expr_lit_int(g, node, dest, false),
        AstExpr::LitFloatExpr(node) => gen_expr_lit_float(g, node, dest),
        AstExpr::LitStrExpr(node) => gen_expr_lit_string(g, node, dest),
        AstExpr::LitBoolExpr(node) => gen_expr_lit_bool(g, node, dest),
        AstExpr::PathExpr(node) => gen_expr_path(g, node, dest),
        AstExpr::CallExpr(node) => gen_expr_call(g, node, dest),
        AstExpr::ThisExpr(node) => gen_expr_self(g, node, dest),
        AstExpr::AsExpr(node) => gen_expr_as(g, node, dest),
        AstExpr::IsExpr(node) => gen_expr_is(g, node, dest),
        AstExpr::TupleExpr(node) => gen_expr_tuple(g, node, dest),
        AstExpr::ParenExpr(node) => gen_expr(g, node.expr().unwrap(), dest),
        AstExpr::MatchExpr(node) => gen_match(g, node, dest),
        AstExpr::LambdaExpr(node) => gen_expr_lambda(g, node, dest),
        AstExpr::ForExpr(node) => gen_expr_for(g, node, dest),
        AstExpr::WhileExpr(node) => gen_expr_while(g, node, dest),
        AstExpr::BreakExpr(node) => gen_expr_break(g, node, dest),
        AstExpr::ContinueExpr(node) => gen_expr_continue(g, node, dest),
        AstExpr::ReturnExpr(node) => gen_expr_return(g, node, dest),
        AstExpr::MethodCallExpr(node) => gen_expr_method_call(g, node, dest),
        AstExpr::Error(_) => unreachable!(),
    }
}

pub(super) fn emit_invoke_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let reg = g.ensure_unit_register();
        g.builder.emit_invoke_direct(reg, callee_id, location);
    } else {
        g.builder
            .emit_invoke_direct(return_reg, callee_id, location);
    }
}

pub(super) fn emit_invoke_generic_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder
            .emit_invoke_generic_direct(dest, callee_id, location);
    } else {
        g.builder
            .emit_invoke_generic_direct(return_reg, callee_id, location);
    }
}

pub(super) fn ensure_register(
    g: &mut AstBytecodeGen,
    dest: DataDest,
    ty: BytecodeType,
) -> Register {
    match dest {
        DataDest::Alloc => g.alloc_temp(ty),
        DataDest::Reg(reg) => reg,
    }
}

pub(super) fn add_const_pool_entry_for_call(
    g: &mut AstBytecodeGen,
    fct: &FctDefinition,
    call_type: &CallType,
) -> ConstPoolIdx {
    match call_type {
        CallType::GenericStaticMethod(id, .., trait_type_params, fct_type_params)
        | CallType::GenericMethod(id, .., trait_type_params, fct_type_params) => {
            g.builder.add_const(ConstPoolEntry::Generic(
                id.index() as u32,
                g.emitter.convert_function_id(fct.id()),
                g.convert_tya(&trait_type_params),
                g.convert_tya(&fct_type_params),
            ))
        }
        CallType::GenericMethodSelf(_, fct_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethodSelf(_, fct_id, trait_type_params, fct_type_params) => {
            g.builder.add_const(ConstPoolEntry::GenericSelf(
                g.emitter.convert_function_id(*fct_id),
                g.convert_tya(&trait_type_params),
                g.convert_tya(&fct_type_params),
            ))
        }
        CallType::GenericMethodNew {
            object_type,
            trait_ty,
            fct_id,
            fct_type_params,
        } => g.builder.add_const(ConstPoolEntry::GenericNew {
            object_type: g.emitter.convert_ty(object_type.clone()),
            trait_ty: g.emitter.convert_trait_ty(&trait_ty),
            fct_id: g.emitter.convert_function_id(*fct_id),
            fct_type_params: g.convert_tya(fct_type_params),
        }),
        CallType::TraitObjectMethod(trait_object_ty, _) => {
            g.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                g.emitter.convert_ty(trait_object_ty.clone()),
                g.emitter.convert_function_id(fct.id()),
            ))
        }

        CallType::Method(.., type_params)
        | CallType::Expr(.., type_params)
        | CallType::Fct(.., type_params) => {
            assert_eq!(
                fct.type_param_definition.type_param_count(),
                type_params.len()
            );
            g.builder.add_const_fct_types(
                g.emitter.convert_function_id(fct.id()),
                g.convert_tya(&type_params),
            )
        }

        _ => panic!("unexpected call type {:?}", call_type),
    }
}

pub(super) fn specialize_type_for_call(
    g: &AstBytecodeGen,
    call_type: &CallType,
    ty: SourceType,
) -> SourceType {
    match call_type {
        CallType::Fct(_, type_params)
        | CallType::Expr(_, _, type_params)
        | CallType::Method(_, _, type_params) => specialize_type(g.sa, ty, type_params),

        CallType::TraitObjectMethod(trait_ty, _actual_object_ty) => {
            let (trait_id, type_params, assoc_types) = match trait_ty {
                SourceType::TraitObject(trait_id, type_params, assoc_types) => {
                    (*trait_id, type_params, assoc_types)
                }
                _ => unreachable!(),
            };
            specialize_ty_for_trait_object(g.sa, ty, trait_id, type_params, assoc_types)
        }
        CallType::GenericMethod(id, _trait_id, _method_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethod(
            id,
            _trait_id,
            _method_id,
            trait_type_params,
            fct_type_params,
        ) => replace_type(
            g.sa,
            ty,
            Some(&trait_type_params.connect(fct_type_params)),
            Some(SourceType::TypeParam(*id)),
        ),

        CallType::GenericMethodSelf(_trait_id, _fct_id, trait_type_params, fct_type_params)
        | CallType::GenericStaticMethodSelf(
            _trait_id,
            _fct_id,
            trait_type_params,
            fct_type_params,
        ) => replace_type(
            g.sa,
            ty,
            Some(&trait_type_params.connect(fct_type_params)),
            None,
        ),

        CallType::GenericMethodNew {
            trait_ty,
            fct_type_params,
            ..
        } => replace_type(
            g.sa,
            ty,
            Some(&trait_ty.type_params.connect(fct_type_params)),
            None,
        ),

        CallType::Lambda(..)
        | CallType::NewClass(..)
        | CallType::NewStruct(..)
        | CallType::NewEnum(..)
        | CallType::Intrinsic(..) => unreachable!(),
    }
}
