use dora_parser::Span;
use std::collections::HashMap;

use self::bytecode::BytecodeBuilder;
use self::expr::{gen_stmt_expr, gen_stmt_let};
use crate::program_emitter::Emitter;
use crate::sema::{
    AnalysisData, ContextFieldId, ExprMapId, FctDefinitionId, FieldIndex, Intrinsic,
    LazyContextData, ScopeId, Sema, SourceFileId, Stmt, StmtId, VarId, new_identity_type_params,
};
use crate::ty::{SourceType, SourceTypeArray};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId, Label, Location, Register};

mod bytecode;
mod expr;
mod function;
mod global;
mod pattern;
#[cfg(test)]
pub mod tests;

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
    context_data: LazyContextData,
    register: Option<Register>,
}

struct AstBytecodeGen<'a> {
    sa: &'a Sema,
    #[allow(unused)]
    emitter: &'a mut Emitter,
    type_params_len: usize,
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
        let context_data = self.analysis.function_context_data();
        self.enter_context(context_data);
    }

    fn enter_block_context(&mut self, id: crate::sema::ExprId) {
        let context_data = self
            .analysis
            .get_block_context(id)
            .expect("missing context");
        self.enter_context(context_data);
    }

    fn enter_context(&mut self, context_data: LazyContextData) {
        let register = if context_data.has_class_id() {
            Some(self.create_context(context_data.clone()))
        } else {
            None
        };

        self.entered_contexts.push(EnteredContext {
            context_data,
            register,
        });
    }

    fn leave_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.leave_context(context_data);
    }

    fn leave_block_context(&mut self, id: crate::sema::ExprId) {
        let context_data = self
            .analysis
            .get_block_context(id)
            .expect("missing context");
        self.leave_context(context_data);
    }

    fn leave_context(&mut self, context_data: LazyContextData) {
        let entered_context = self.entered_contexts.pop().expect("missing context");

        if context_data.has_class_id() {
            assert!(entered_context.register.is_some());
        } else {
            assert!(entered_context.register.is_none());
        }
    }

    fn create_context(&mut self, context_data: LazyContextData) -> Register {
        let class_id = context_data.class_id();

        let context_register = self.builder.alloc_global(BytecodeType::Ptr);
        let idx = self.builder.add_const_cls_types(
            self.emitter.convert_class_id(class_id),
            self.emitter.convert_tya(&self.identity_type_params()),
        );
        self.builder
            .emit_new_object(context_register, idx, self.loc(self.span));

        if context_data.has_parent_slot() {
            // Load context field of lambda object in self.
            let temp_parent_context_reg = self.alloc_temp(BytecodeType::Ptr);

            let parent_context_reg = if let Some(parent_context_reg) = last_context_register(self) {
                parent_context_reg
            } else {
                let self_reg = var_reg(self, SELF_VAR_ID);

                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    self.emitter.convert_class_id(lambda_cls_id),
                    BytecodeTypeArray::empty(),
                    0,
                );
                self.builder.emit_load_field(
                    temp_parent_context_reg,
                    self_reg,
                    idx,
                    self.loc(self.span),
                );

                temp_parent_context_reg
            };

            // Store value in parent field of context object.
            assert!(context_data.has_parent_slot());
            let idx = self.builder.add_const_field_types(
                self.emitter.convert_class_id(class_id),
                self.emitter.convert_tya(&self.identity_type_params()),
                0,
            );
            self.builder.emit_store_field(
                parent_context_reg,
                context_register,
                idx,
                self.loc(self.span),
            );

            self.free_temp(temp_parent_context_reg);
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
        let bty: BytecodeType = self.emitter.convert_ty_reg(var.ty.clone());
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

    fn identity_type_params(&self) -> SourceTypeArray {
        new_identity_type_params(0, self.type_params_len)
    }

    fn convert_tya(&self, ty: &SourceTypeArray) -> BytecodeTypeArray {
        self.emitter.convert_tya(&ty)
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
        assert!(!ty.is_class());
        self.builder.alloc_var(ty)
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
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
    let register_bty = g.emitter.convert_ty_reg(return_type.clone());
    let dest_reg = g.alloc_temp(register_bty);
    let msg_reg = g.alloc_temp(BytecodeType::Ptr);
    g.builder.emit_const_string(msg_reg, msg.to_string());
    g.builder.emit_push_register(msg_reg);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_idx = g.builder.add_const_fct_types(
        FunctionId(
            g.sa.known
                .functions
                .fatal_error()
                .index()
                .try_into()
                .expect("overflow"),
        ),
        fct_type_params,
    );
    g.builder.emit_invoke_direct(dest_reg, fct_idx, g.loc(span));
    g.builder.emit_ret(dest_reg);
    g.free_temp(dest_reg);
    g.free_temp(msg_reg);
}

fn last_context_register(g: &AstBytecodeGen) -> Option<Register> {
    g.entered_contexts.iter().rev().find_map(|ec| ec.register)
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
    let context_data = entered_context.context_data.clone();
    let cls_id = context_data.class_id();
    let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
    let field_idx = g.builder.add_const_field_types(
        g.emitter.convert_class_id(cls_id),
        g.convert_tya(&g.identity_type_params()),
        field_id.0 as u32,
    );
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
