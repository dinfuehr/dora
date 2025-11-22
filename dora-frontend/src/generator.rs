use dora_parser::Span;
use dora_parser::ast::{self, AstId, SyntaxNodeBase};
use std::collections::HashMap;

use self::bytecode::BytecodeBuilder;
use self::expr::{
    gen_expr, gen_stmt_expr, gen_stmt_let, last_context_register, set_var_reg, store_in_context,
    var_reg,
};
use crate::program_emitter::Emitter;
use crate::sema::{
    AnalysisData, ContextFieldId, FctDefinitionId, FieldIndex, Intrinsic, LazyContextData, Sema,
    SourceFileId, VarId, new_identity_type_params,
};
use crate::ty::{SourceType, SourceTypeArray};
use dora_bytecode::{BytecodeFunction, BytecodeType, BytecodeTypeArray, Label, Location, Register};

mod bytecode;
mod expr;
mod function;
mod pattern;
#[cfg(test)]
pub mod tests;

pub use self::function::{generate_fct, generate_fct_id, generate_global_initializer};

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

    fn loc_id(&self, ast_id: AstId) -> Location {
        self.loc(self.span(ast_id))
    }

    fn ast_file(&self) -> &'a ast::File {
        self.sa.file(self.file_id).ast()
    }

    fn span(&self, ast_id: AstId) -> Span {
        self.node(ast_id).span()
    }

    fn node(&self, ast_id: AstId) -> &'a ast::Ast {
        self.sa.node(self.file_id, ast_id)
    }

    #[allow(unused)]
    fn node2<T: SyntaxNodeBase>(&self, ast_id: AstId) -> T {
        self.sa.file(self.file_id).ast().node2(ast_id)
    }

    fn generate_global_initializer(mut self, expr: ast::AstExpr) -> BytecodeFunction {
        self.push_scope();
        self.builder.set_params(Vec::new());
        self.enter_function_context();
        self.emit_global_initializer(expr);
        self.leave_function_context();
        self.pop_scope();
        self.builder.generate()
    }

    fn emit_global_initializer(&mut self, expr: ast::AstExpr) {
        let result = gen_expr(self, expr, DataDest::Alloc);
        self.builder.emit_ret(result);
        self.free_if_temp(result);
    }

    fn enter_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.enter_context(context_data);
    }

    fn enter_block_context(&mut self, id: ast::AstId) {
        let context_data = self
            .analysis
            .map_block_contexts
            .get(id)
            .cloned()
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

    fn leave_block_context(&mut self, id: ast::AstId) {
        let context_data = self
            .analysis
            .map_block_contexts
            .get(id)
            .cloned()
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

    fn visit_stmt(&mut self, stmt_id: ast::AstId) {
        let stmt = self.node2::<ast::AstStmt>(stmt_id);
        match stmt {
            ast::AstStmt::ExprStmt(expr) => gen_stmt_expr(self, expr),
            ast::AstStmt::Let(stmt) => gen_stmt_let(self, stmt),
            ast::AstStmt::Error(_) => unreachable!(),
        }
    }

    fn allocate_register_for_var(&mut self, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);
        let bty: BytecodeType = self.emitter.convert_ty_reg(var.ty.clone());
        let reg = self.alloc_var(bty);
        set_var_reg(self, var_id, reg);
    }

    fn ty(&self, id: ast::AstId) -> SourceType {
        self.analysis.ty(id)
    }

    fn get_intrinsic(&self, id: ast::AstId) -> Option<IntrinsicInfo> {
        let call_type = self.analysis.map_calls.get(id).expect("missing CallType");

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
