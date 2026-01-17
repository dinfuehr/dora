use dora_bytecode::{BytecodeType, ConstPoolEntry, ConstPoolIdx, Location, Register};
use dora_parser::ast::{AstExpr, SyntaxNodeBase};

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
use crate::sema::{CallType, Expr, ExprId, FctDefinition};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::SourceType;

/// Trait for arguments that can be passed to gen_expr.
/// This allows gen_expr to accept both AstExpr and ExprId.
pub(super) trait GenExprArg {
    fn to_expr_id(&self, g: &AstBytecodeGen) -> ExprId;
    fn to_ast_expr(&self, g: &AstBytecodeGen) -> AstExpr;
}

impl GenExprArg for AstExpr {
    fn to_expr_id(&self, g: &AstBytecodeGen) -> ExprId {
        g.analysis.exprs().to_expr_id(self.id())
    }

    fn to_ast_expr(&self, _g: &AstBytecodeGen) -> AstExpr {
        self.clone()
    }
}

impl GenExprArg for ExprId {
    fn to_expr_id(&self, _g: &AstBytecodeGen) -> ExprId {
        *self
    }

    fn to_ast_expr(&self, g: &AstBytecodeGen) -> AstExpr {
        let syntax_node_id = g.analysis.exprs().syntax_node_id(*self);
        g.sa.syntax_by_id(g.file_id, syntax_node_id)
    }
}

pub(super) fn gen_expr<E: GenExprArg>(
    g: &mut AstBytecodeGen,
    expr_arg: E,
    dest: DataDest,
) -> Register {
    let expr_id = expr_arg.to_expr_id(g);
    let ast_expr = expr_arg.to_ast_expr(g);
    let expr = g.analysis.expr(expr_id);

    match (&ast_expr, expr) {
        (AstExpr::UnExpr(_), Expr::Un(e)) => gen_expr_un(g, expr_id, e, dest),
        (AstExpr::AssignExpr(_), Expr::Assign(e)) => {
            self::assign::gen_expr_assign(g, expr_id, e, dest)
        }
        (AstExpr::BinExpr(node), Expr::Bin(e)) => gen_expr_bin(g, expr_id, e, node.clone(), dest),
        (AstExpr::FieldExpr(node), Expr::Field(e)) => {
            gen_expr_field(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::BlockExpr(_), Expr::Block(e)) => gen_expr_block(g, expr_id, e, dest),
        (AstExpr::IfExpr(_), Expr::If(e)) => gen_expr_if(g, expr_id, e, dest),
        (AstExpr::TemplateExpr(node), Expr::Template(e)) => {
            gen_expr_template(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitCharExpr(node), Expr::LitChar(e)) => {
            gen_expr_lit_char(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitIntExpr(node), Expr::LitInt(e)) => {
            gen_expr_lit_int(g, expr_id, e, node.clone(), dest, false)
        }
        (AstExpr::LitFloatExpr(node), Expr::LitFloat(e)) => {
            gen_expr_lit_float(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitStrExpr(node), Expr::LitStr(e)) => {
            gen_expr_lit_string(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitBoolExpr(node), Expr::LitBool(e)) => {
            gen_expr_lit_bool(g, expr_id, *e, node.clone(), dest)
        }
        (AstExpr::PathExpr(node), Expr::Name(e)) => {
            gen_expr_path(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::CallExpr(node), Expr::Call(e)) => {
            gen_expr_call(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ThisExpr(_), Expr::This) => gen_expr_self(g, expr_id, dest),
        (AstExpr::AsExpr(node), Expr::As(e)) => gen_expr_as(g, expr_id, e, node.clone(), dest),
        (AstExpr::IsExpr(node), Expr::Is(e)) => gen_expr_is(g, expr_id, e, node.clone(), dest),
        (AstExpr::TupleExpr(node), Expr::Tuple(e)) => {
            gen_expr_tuple(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ParenExpr(node), Expr::Paren(inner_expr_id)) => {
            let inner_ast = node.expr().unwrap();
            let inner_expr = g.analysis.expr(*inner_expr_id);
            gen_expr_paren(g, *inner_expr_id, inner_expr, inner_ast, dest)
        }
        (AstExpr::MatchExpr(node), Expr::Match(e)) => gen_match(g, expr_id, e, node.clone(), dest),
        (AstExpr::LambdaExpr(node), Expr::Lambda(e)) => {
            gen_expr_lambda(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ForExpr(_), Expr::For(e)) => gen_expr_for(g, expr_id, e, dest),
        (AstExpr::WhileExpr(_), Expr::While(e)) => gen_expr_while(g, expr_id, e, dest),
        (AstExpr::BreakExpr(_), Expr::Break) => gen_expr_break(g, expr_id, dest),
        (AstExpr::ContinueExpr(_), Expr::Continue) => gen_expr_continue(g, expr_id, dest),
        (AstExpr::ReturnExpr(_), Expr::Return(e)) => gen_expr_return(g, expr_id, e, dest),
        (AstExpr::MethodCallExpr(node), Expr::MethodCall(e)) => {
            gen_expr_method_call(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::Error(_), Expr::Error) => unreachable!(),
        _ => unreachable!("mismatched AST and IR expression types"),
    }
}

fn gen_expr_paren(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    expr: &Expr,
    ast_expr: AstExpr,
    dest: DataDest,
) -> Register {
    match (&ast_expr, expr) {
        (AstExpr::UnExpr(_), Expr::Un(e)) => gen_expr_un(g, expr_id, e, dest),
        (AstExpr::AssignExpr(_), Expr::Assign(e)) => {
            self::assign::gen_expr_assign(g, expr_id, e, dest)
        }
        (AstExpr::BinExpr(node), Expr::Bin(e)) => gen_expr_bin(g, expr_id, e, node.clone(), dest),
        (AstExpr::FieldExpr(node), Expr::Field(e)) => {
            gen_expr_field(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::BlockExpr(_), Expr::Block(e)) => gen_expr_block(g, expr_id, e, dest),
        (AstExpr::IfExpr(_), Expr::If(e)) => gen_expr_if(g, expr_id, e, dest),
        (AstExpr::TemplateExpr(node), Expr::Template(e)) => {
            gen_expr_template(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitCharExpr(node), Expr::LitChar(e)) => {
            gen_expr_lit_char(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitIntExpr(node), Expr::LitInt(e)) => {
            gen_expr_lit_int(g, expr_id, e, node.clone(), dest, false)
        }
        (AstExpr::LitFloatExpr(node), Expr::LitFloat(e)) => {
            gen_expr_lit_float(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitStrExpr(node), Expr::LitStr(e)) => {
            gen_expr_lit_string(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::LitBoolExpr(node), Expr::LitBool(e)) => {
            gen_expr_lit_bool(g, expr_id, *e, node.clone(), dest)
        }
        (AstExpr::PathExpr(node), Expr::Name(e)) => {
            gen_expr_path(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::CallExpr(node), Expr::Call(e)) => {
            gen_expr_call(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ThisExpr(_), Expr::This) => gen_expr_self(g, expr_id, dest),
        (AstExpr::AsExpr(node), Expr::As(e)) => gen_expr_as(g, expr_id, e, node.clone(), dest),
        (AstExpr::IsExpr(node), Expr::Is(e)) => gen_expr_is(g, expr_id, e, node.clone(), dest),
        (AstExpr::TupleExpr(node), Expr::Tuple(e)) => {
            gen_expr_tuple(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ParenExpr(node), Expr::Paren(inner_expr_id)) => {
            let inner_ast = node.expr().unwrap();
            let inner_expr = g.analysis.expr(*inner_expr_id);
            gen_expr_paren(g, *inner_expr_id, inner_expr, inner_ast, dest)
        }
        (AstExpr::MatchExpr(node), Expr::Match(e)) => gen_match(g, expr_id, e, node.clone(), dest),
        (AstExpr::LambdaExpr(node), Expr::Lambda(e)) => {
            gen_expr_lambda(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::ForExpr(_), Expr::For(e)) => gen_expr_for(g, expr_id, e, dest),
        (AstExpr::WhileExpr(_), Expr::While(e)) => gen_expr_while(g, expr_id, e, dest),
        (AstExpr::BreakExpr(_), Expr::Break) => gen_expr_break(g, expr_id, dest),
        (AstExpr::ContinueExpr(_), Expr::Continue) => gen_expr_continue(g, expr_id, dest),
        (AstExpr::ReturnExpr(_), Expr::Return(e)) => gen_expr_return(g, expr_id, e, dest),
        (AstExpr::MethodCallExpr(node), Expr::MethodCall(e)) => {
            gen_expr_method_call(g, expr_id, e, node.clone(), dest)
        }
        (AstExpr::Error(_), Expr::Error) => unreachable!(),
        _ => unreachable!("mismatched AST and IR expression types"),
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
