use dora_parser::Span;
use std::collections::HashMap;

use self::bytecode::BytecodeBuilder;
use self::expr::{gen_stmt_expr, gen_stmt_let};
use crate::program_emitter::Emitter;
use crate::sema::{
    AnalysisData, ContextFieldId, ContextId, ExprMapId, FctDefinitionId, FieldIndex, Intrinsic,
    ScopeId, Sema, SourceFileId, Stmt, StmtId, TypeParamDefinitionId, VarId,
    generated_identity_type_params, lambda_object_type,
};
use crate::ty::{SourceType, SourceTypeArray};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, Label, Location, Register};

mod bytecode;
mod expr;
mod function;
mod global;
mod int_dispatch;
mod pattern;

pub use self::function::{generate_fct, generate_fct_id};
pub use self::global::generate_global_initializer;

pub struct LoopLabels {
    cond: Label,
    end: Label,
}

impl LoopLabels {
    fn new(cond: Label, end: Label) -> LoopLabels {
        LoopLabels { cond, end }
    }
}

const SELF_VAR_ID: VarId = VarId(0);

struct EnteredContext {
    context_id: ContextId,
    register: Option<Register>,
}

struct AstBytecodeGen<'a> {
    sa: &'a Sema,
    #[allow(unused)]
    emitter: &'a mut Emitter,
    frontend_type_params_len: usize,
    type_params_len: usize,
    type_param_definition_id: TypeParamDefinitionId,
    is_lambda: bool,
    return_type: SourceType,
    file_id: SourceFileId,
    span: Span,
    analysis: &'a AnalysisData,

    builder: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
    entered_contexts: Vec<EnteredContext>,
    unit_register: Option<Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn loc(&self, span: Span) -> Location {
        self.sa.compute_loc(self.file_id, span)
    }

    fn loc_for_expr(&self, id: crate::sema::ExprId) -> Location {
        self.loc(self.span_for_expr(id))
    }

    fn span_for_expr(&self, id: crate::sema::ExprId) -> Span {
        let ptr = self.analysis.exprs().syntax_node_ptr(id);
        let node = self
            .sa
            .syntax::<dora_parser::ast::SyntaxNode>(self.file_id, ptr);
        node.span()
    }

    fn loc_for_pattern(&self, id: crate::sema::PatternId) -> Location {
        self.loc(self.span_for_pattern(id))
    }

    fn span_for_pattern(&self, id: crate::sema::PatternId) -> Span {
        let ptr = self.analysis.patterns().syntax_node_ptr(id);
        let node = self
            .sa
            .syntax::<dora_parser::ast::SyntaxNode>(self.file_id, ptr);
        node.span()
    }

    fn enter_function_context(&mut self) {
        let context_id = self.analysis.function_context_id();
        self.enter_context(context_id);
    }

    fn enter_block_context(&mut self, id: crate::sema::ExprId) {
        let context_id = self
            .analysis
            .get_block_context_id(id)
            .expect("missing context");
        self.enter_context(context_id);
    }

    fn enter_context(&mut self, context_id: ContextId) {
        let register = if self.sa.context(context_id).has_class_id() {
            Some(self.create_context(context_id))
        } else {
            None
        };

        self.entered_contexts.push(EnteredContext {
            context_id,
            register,
        });
    }

    fn leave_function_context(&mut self) {
        let context_id = self.analysis.function_context_id();
        self.leave_context(context_id);
    }

    fn leave_block_context(&mut self, id: crate::sema::ExprId) {
        let context_id = self
            .analysis
            .get_block_context_id(id)
            .expect("missing context");
        self.leave_context(context_id);
    }

    fn leave_context(&mut self, context_id: ContextId) {
        let entered_context = self.entered_contexts.pop().expect("missing context");
        assert_eq!(entered_context.context_id, context_id);

        if self.sa.context(context_id).has_class_id() {
            assert!(entered_context.register.is_some());
        } else {
            assert!(entered_context.register.is_none());
        }
    }

    fn create_context(&mut self, context_id: ContextId) -> Register {
        let context = self.sa.context(context_id);
        let has_parent_slot = context.has_parent_slot();

        let context_type = self.context_type(context_id);
        let (bc_class_id, bc_type_params) = match &context_type {
            BytecodeType::Class(class_id, type_params) => (*class_id, type_params.clone()),
            _ => unreachable!(),
        };
        let context_register = self.builder.alloc_global(context_type);
        let idx = self
            .builder
            .add_const_cls_types(bc_class_id, bc_type_params.clone());
        self.builder
            .emit_new_object(context_register, idx, &[], self.loc(self.span));

        if has_parent_slot {
            let parent_context_reg = if let Some(parent_context_reg) = last_context_register(self) {
                parent_context_reg
            } else {
                assert!(self.is_lambda);
                let parent_context_id = enclosing_context_class(self.sa, context_id);
                let location = self.loc(self.span);
                load_outer_context_object(self, parent_context_id, location)
            };

            // Store value in parent field of context object.
            let idx = self
                .builder
                .add_const_field_types(bc_class_id, bc_type_params, 0);
            self.builder.emit_store_field(
                parent_context_reg,
                context_register,
                idx,
                self.loc(self.span),
            );
            self.free_if_temp(parent_context_reg);
        }

        context_register
    }

    fn visit_stmt(&mut self, stmt_id: StmtId) {
        let stmt = self.analysis.stmt(stmt_id);
        match stmt {
            Stmt::Expr(expr_id) => gen_stmt_expr(self, *expr_id),
            Stmt::Let(let_stmt) => gen_stmt_let(self, let_stmt),
            Stmt::Error => unreachable!(),
        }
    }

    fn allocate_register_for_var(&mut self, var_id: VarId) {
        let vars = self.analysis.vars();
        let var = vars.get_var(var_id);
        let bty: BytecodeType = self.emitter.convert_ty(self.sa, var.ty.clone());
        let reg = self.alloc_var(bty);
        set_var_reg(self, var_id, reg);
    }

    fn ty<T: ExprMapId>(&self, id: T) -> SourceType {
        self.analysis.ty(id)
    }

    fn get_intrinsic<T: ExprMapId>(&self, id: T) -> Option<IntrinsicInfo> {
        let call_type = self.analysis.get_call_type(id).expect("missing CallType");

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic.into());
        }

        let fid = if let Some(fct_id) = call_type.fct_id() {
            fct_id
        } else {
            return None;
        };

        let fct = self.sa.fct(fid);

        if let Some(intrinsic) = fct.intrinsic.get().cloned() {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn type_params_for_generated(&self, needs_self_type_param: bool) -> SourceTypeArray {
        let type_params = generated_identity_type_params(
            self.sa,
            self.sa.type_param_definition(self.type_param_definition_id),
            needs_self_type_param,
        );
        assert!(
            type_params.len() == self.type_params_len
                || type_params.len() == self.type_params_len + 1
        );
        type_params
    }

    fn convert_tya(&mut self, ty: &SourceTypeArray) -> BytecodeTypeArray {
        self.emitter.convert_tya(self.sa, &ty)
    }

    fn context_type(&mut self, context_id: ContextId) -> BytecodeType {
        let context = self.sa.context(context_id);
        let class = self.sa.class(context.class_id());
        let class_id = self.emitter.convert_class_id(self.sa, class.id());
        let type_params = self.type_params_for_generated(class.needs_self_type_param);
        let type_params = self.convert_tya(&type_params);
        BytecodeType::Class(class_id, type_params)
    }

    fn lambda_object_type(&mut self) -> BytecodeType {
        let ty = lambda_object_type(self.sa, self.analysis, self.frontend_type_params_len);
        self.emitter.convert_ty(self.sa, ty)
    }

    fn ensure_unit_register(&mut self) -> Register {
        if let Some(register) = self.unit_register {
            return register;
        }

        let register = self.builder.alloc_global(BytecodeType::Unit);
        self.unit_register = Some(register);
        register
    }

    fn push_scope(&mut self) {
        self.builder.push_scope();
    }

    fn pop_scope(&mut self) {
        self.builder.pop_scope();
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        self.builder.alloc_var(ty)
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        self.builder.alloc_temp(ty)
    }

    fn free_if_temp(&mut self, reg: Register) {
        self.builder.free_if_temp(reg);
    }

    fn free_temp(&mut self, reg: Register) {
        self.builder.free_temp(reg);
    }
}

struct IntrinsicInfo {
    intrinsic: Intrinsic,
    fct_id: Option<FctDefinitionId>,
}

impl IntrinsicInfo {
    fn with_fct(intrinsic: Intrinsic, fct_id: FctDefinitionId) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: Some(fct_id),
        }
    }
}

impl From<Intrinsic> for IntrinsicInfo {
    fn from(intrinsic: Intrinsic) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: None,
        }
    }
}

fn gen_fatal_error(g: &mut AstBytecodeGen, msg: &str, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty(g.sa, return_type.clone());
    let dest_reg = g.alloc_temp(register_bty);
    let string_ty = SourceType::Class(g.sa.known.classes.string(), SourceTypeArray::empty());
    let string_ty = g.emitter.convert_ty(g.sa, string_ty);
    let msg_reg = g.alloc_temp(string_ty);
    g.builder.emit_const_string(msg_reg, msg.to_string());
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_id = g
        .emitter
        .convert_function_id(g.sa, g.sa.known.functions.fatal_error());
    let fct_idx = g.builder.add_const_fct_types(fct_id, fct_type_params);
    g.builder
        .emit_invoke_direct(dest_reg, fct_idx, &[msg_reg], g.loc(span));
    g.builder.emit_ret(dest_reg);
    g.free_temp(dest_reg);
    g.free_temp(msg_reg);
}

fn gen_unreachable(g: &mut AstBytecodeGen, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty(g.sa, return_type.clone());
    let dest = g.alloc_temp(register_bty);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_id = g
        .emitter
        .convert_function_id(g.sa, g.sa.known.functions.unreachable());
    let fct_idx = g.builder.add_const_fct_types(fct_id, fct_type_params);
    g.builder
        .emit_invoke_direct(dest, fct_idx, &[], g.loc(span));
    g.builder.emit_ret(dest);
    g.free_temp(dest);
}

fn last_context_register(g: &AstBytecodeGen) -> Option<Register> {
    g.entered_contexts.iter().rev().find_map(|ec| ec.register)
}

fn load_outer_context_object(
    g: &mut AstBytecodeGen,
    context_id: ContextId,
    location: Location,
) -> Register {
    assert!(g.is_lambda);
    assert!(g.sa.context(context_id).has_class_id());

    let function_context_id = g.analysis.function_context_id();
    let mut current_context_id =
        g.sa.context(function_context_id)
            .parent()
            .expect("missing outer context");
    let mut context_chain = Vec::new();

    loop {
        let context = g.sa.context(current_context_id);
        if context.has_class_id() {
            context_chain.push(current_context_id);

            if current_context_id == context_id {
                break;
            }
        }

        current_context_id = context.parent().expect("context is not an ancestor");
    }

    let innermost_context = context_chain[0];
    let innermost_type = g.context_type(innermost_context);
    let mut context_register = g.alloc_temp(innermost_type);

    let lambda_object_type = g.lambda_object_type();
    let BytecodeType::Class(lambda_class_id, lambda_type_params) = lambda_object_type else {
        unreachable!();
    };
    let field_idx = g
        .builder
        .add_const_field_types(lambda_class_id, lambda_type_params, 0);
    g.builder.emit_load_field(
        context_register,
        var_reg(g, SELF_VAR_ID),
        field_idx,
        location,
    );

    for context_idx in 0..context_chain.len() - 1 {
        let context_id = context_chain[context_idx];
        assert!(g.sa.context(context_id).has_parent_slot());

        let context_type = g.context_type(context_id);
        let (class_id, type_params) = match context_type {
            BytecodeType::Class(class_id, type_params) => (class_id, type_params),
            _ => unreachable!(),
        };
        let field_idx = g.builder.add_const_field_types(class_id, type_params, 0);

        let parent_type = g.context_type(context_chain[context_idx + 1]);
        let parent_register = g.alloc_temp(parent_type);
        g.builder
            .emit_load_field(parent_register, context_register, field_idx, location);
        g.free_temp(context_register);
        context_register = parent_register;
    }

    context_register
}

fn enclosing_context_class(sa: &Sema, context_id: ContextId) -> ContextId {
    let mut parent_id = sa
        .context(context_id)
        .parent()
        .expect("missing parent context");

    while !sa.context(parent_id).has_class_id() {
        parent_id = sa
            .context(parent_id)
            .parent()
            .expect("missing parent context class");
    }

    parent_id
}

fn emit_mov(g: &mut AstBytecodeGen, dest: Register, src: Register) {
    if dest != src {
        g.builder.emit_mov(dest, src);
    }
}

fn store_in_context(
    g: &mut AstBytecodeGen,
    src: Register,
    scope_id: ScopeId,
    field_id: ContextFieldId,
    location: Location,
) {
    let entered_context = &g.entered_contexts[scope_id.0];
    let context_register = entered_context.register.expect("missing register");
    let context = g.sa.context(entered_context.context_id);
    let cls_id = context.class_id();
    let cls = g.sa.class(cls_id);
    let field_id = field_id_from_context_idx(field_id, context.has_parent_slot());
    let bc_cls_id = g.emitter.convert_class_id(g.sa, cls_id);
    let type_params = g.type_params_for_generated(cls.needs_self_type_param);
    let bc_type_params = g.convert_tya(&type_params);
    let field_idx = g
        .builder
        .add_const_field_types(bc_cls_id, bc_type_params, field_id.0 as u32);
    g.builder
        .emit_store_field(src, context_register, field_idx, location);
}

fn var_reg(g: &AstBytecodeGen, var_id: VarId) -> Register {
    *g.var_registers
        .get(&var_id)
        .expect("no register for var found")
}

fn set_var_reg(g: &mut AstBytecodeGen, var_id: VarId, reg: Register) {
    let old = g.var_registers.insert(var_id, reg);
    assert!(old.is_none());
}

#[derive(Copy, Clone, Debug)]
enum DataDest {
    // Allocate a new register and store result in it.
    Alloc,

    // Store the result in the given register.
    Reg(Register),
}

impl DataDest {
    fn is_alloc(&self) -> bool {
        match self {
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn reg(&self) -> Register {
        match self {
            DataDest::Alloc => panic!("not a register"),
            DataDest::Reg(reg) => *reg,
        }
    }
}

pub(super) fn field_id_from_context_idx(
    context_idx: ContextFieldId,
    has_outer_context_slot: bool,
) -> FieldIndex {
    let start_idx = if has_outer_context_slot { 1 } else { 0 };
    let ContextFieldId(context_idx) = context_idx;
    FieldIndex(start_idx + context_idx)
}
