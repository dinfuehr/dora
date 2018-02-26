use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::ptr;

use libc;

use baseline::fct::JitFct;
use class::TypeParams;
use ctxt::{Fct, FctKind, FctParent, FctSrc, IdentType, Intrinsic, SemContext, VarId};
use opt::fct::JitOptFct;
use ty::{BuiltinType, MachineMode};

use dora_parser::ast::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use llvm::*;
use llvm::prelude::*;
use llvm::analysis::*;
use llvm::core::*;
use llvm::orc::*;
use llvm::transforms::scalar::*;

pub mod util;
pub mod fct;

pub fn generate_fct<'ast>(
    ctxt: &SemContext<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> Result<*const u8, ()> {
    let name = fct.full_name(ctxt);

    let jit_fct = {
        let mut cg = CodeGen {
            ctxt: ctxt,
            fct: fct,
            ast: fct.ast,
            src: src,
            cls_type_params,
            fct_type_params,

            fct_name: CString::new(name).unwrap(),

            context: ptr::null_mut(),
            module: ptr::null_mut(),
            shared_module: ptr::null_mut(),
            builder: ptr::null_mut(),
            function: ptr::null_mut(),

            break_label: ptr::null_mut(),
            continue_label: ptr::null_mut(),

            map_vars: HashMap::new(),
        };

        cg.generate()?
    };

    let fct_start = jit_fct.fct_ptr();

    {
        let mut specials = src.specializations.write().unwrap();
        let key = (cls_type_params.clone(), fct_type_params.clone());

        let jit_fct_id = ctxt.jit_fcts.len().into();
        ctxt.jit_fcts.push(JitFct::Opt(jit_fct));
        specials.insert(key, jit_fct_id);
    }

    Ok(fct_start)
}

type EmitResult<T> = Result<T, ()>;

fn fail<T>() -> EmitResult<T> {
    Err(())
}

fn ok<T>(value: T) -> EmitResult<T> {
    Ok(value)
}

struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    src: &'a mut FctSrc,
    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,

    fct_name: CString,

    context: LLVMContextRef,
    module: LLVMModuleRef,
    shared_module: LLVMSharedModuleRef,
    builder: LLVMBuilderRef,
    function: LLVMValueRef,

    break_label: LLVMBasicBlockRef,
    continue_label: LLVMBasicBlockRef,

    map_vars: HashMap<VarId, LLVMValueRef>,
}

impl<'a, 'ast> CodeGen<'a, 'ast>
where
    'ast: 'a,
{
    fn generate(&mut self) -> EmitResult<JitOptFct> {
        self.init();
        self.create_function()?;
        self.add_entry_bb();
        self.add_params();

        let block = self.ast.block.as_ref().unwrap();
        self.emit_stmt(block)?;

        if !self.block_has_terminator() {
            let always_returns = self.src.always_returns;

            unsafe {
                if always_returns {
                    LLVMBuildUnreachable(self.builder);
                } else {
                    LLVMBuildRetVoid(self.builder);
                }
            }
        }

        unsafe {
            LLVMDisposeBuilder(self.builder);
        }

        if self.ctxt.args.flag_emit_llvm {
            unsafe {
                LLVMDumpModule(self.module);
            }
        }

        self.verify()?;

        let orc = self.ctxt.llvm_jit.orc;
        let mut ptr = 0;

        unsafe {
            // add eagerly compiled IR
            let mut handle = 0;
            let res = LLVMOrcAddEagerlyCompiledIR(
                orc,
                &mut handle,
                self.shared_module,
                resolver,
                ptr::null_mut(),
            );
            assert!(res == LLVMOrcErrorCode::LLVMOrcErrSuccess);

            let pm = LLVMCreateFunctionPassManagerForModule(self.module);
            LLVMAddScalarReplAggregatesPassSSA(pm);
            LLVMInitializeFunctionPassManager(pm);
            LLVMRunFunctionPassManager(pm, self.function);

            if self.ctxt.args.flag_emit_llvm {
                LLVMDumpModule(self.module);
            }

            if LLVMOrcGetSymbolAddress(orc, &mut ptr, self.fct_name.as_ptr()) ==
                LLVMOrcErrorCode::LLVMOrcErrSuccess
            {
                assert!(ptr != 0);

                let jit_fct = JitOptFct {
                    fct_id: self.fct.id,
                    fct_start: ptr as *const _,
                };

                ok(jit_fct)
            } else {
                fail()
            }
        }
    }

    fn init(&mut self) {
        unsafe {
            self.context = LLVMContextCreate();
            self.module = LLVMModuleCreateWithNameInContext(noname(), self.context);
            self.shared_module = LLVMOrcMakeSharedModule(self.module);
            self.builder = LLVMCreateBuilderInContext(self.context);
        }
    }

    fn create_function(&mut self) -> EmitResult<()> {
        let mut params = Vec::with_capacity(self.ast.params.len());

        for param in &self.ast.params {
            let varid = *self.src.map_vars.get(param.id).unwrap();
            let ty = self.ty_var(varid);
            params.push(self.llvm_ty(ty));
        }

        let return_type = self.specialize_type(self.fct.return_type);
        let llvm_return_type = self.llvm_ty(return_type);

        unsafe {
            let function_type = LLVMFunctionType(
                llvm_return_type,
                params.as_mut_ptr(),
                params.len() as u32,
                0,
            );

            self.function = LLVMAddFunction(self.module, self.fct_name.as_ptr(), function_type);
        }

        ok(())
    }

    fn add_entry_bb(&mut self) {
        let bb = self.append_block(b"entry\0");

        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }

    fn add_params(&mut self) {
        for (ind, param) in self.ast.params.iter().enumerate() {
            let varid = *self.src.map_vars.get(param.id).unwrap();
            let ty = self.ty_var(varid);
            let ty = self.llvm_ty(ty);

            unsafe {
                let value = LLVMGetParam(self.function, ind as u32);
                let ptr = LLVMBuildAlloca(self.builder, ty, noname());

                LLVMBuildStore(self.builder, value, ptr);
                self.map_vars.insert(varid, ptr);
            }
        }
    }

    fn verify(&mut self) -> EmitResult<()> {
        unsafe {
            if LLVMVerifyFunction(
                self.function,
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
            ) == 1
            {
                println!("invalid llvm function!");
                fail()
            } else {
                ok(())
            }
        }
    }

    fn emit_stmt(&mut self, s: &'ast Stmt) -> EmitResult<()> {
        match *s {
            StmtExpr(ref stmt) => {
                self.emit_expr(&stmt.expr)?;

                Ok(())
            }

            StmtIf(ref stmt) => self.emit_if(stmt),
            StmtLoop(ref stmt) => self.emit_loop(stmt),
            StmtWhile(ref stmt) => self.emit_while(stmt),
            StmtFor(_) => fail(),
            StmtReturn(ref stmt) => self.emit_return(stmt),
            StmtBreak(_) => fail(),
            StmtContinue(_) => fail(),
            StmtBlock(ref stmt) => self.emit_block(stmt),
            StmtVar(ref stmt) => self.emit_var(stmt),
            StmtThrow(_) => fail(),
            StmtDefer(_) => fail(),
            StmtDo(_) => fail(),
            StmtSpawn(_) => fail(),
        }
    }

    fn emit_if(&mut self, s: &'ast StmtIfType) -> EmitResult<()> {
        let then_block = self.append_block(b"if_then\0");
        let else_block = self.append_block(b"if_else\0");
        let merge_block = self.append_block(b"if_merge\0");

        unsafe {
            let value = self.emit_expr(&s.cond)?;
            LLVMBuildCondBr(self.builder, value, then_block, else_block);

            LLVMPositionBuilderAtEnd(self.builder, then_block);
            self.emit_stmt(&s.then_block)?;

            if !self.block_has_terminator() {
                LLVMBuildBr(self.builder, merge_block);
            }

            if let Some(ref stmt_else_block) = s.else_block {
                LLVMPositionBuilderAtEnd(self.builder, else_block);
                self.emit_stmt(stmt_else_block)?;
                if !self.block_has_terminator() {
                    LLVMBuildBr(self.builder, merge_block);
                }
            }

            LLVMPositionBuilderAtEnd(self.builder, merge_block);
        }

        ok(())
    }

    fn emit_loop(&mut self, s: &'ast StmtLoopType) -> EmitResult<()> {
        let loop_block = self.append_block(b"loop_body\0");
        let merge_block = self.append_block(b"loop_merge\0");

        unsafe {
            LLVMBuildBr(self.builder, loop_block);

            let saved_break_label = self.break_label;
            let saved_continue_label = self.continue_label;

            self.break_label = merge_block;
            self.continue_label = loop_block;

            LLVMPositionBuilderAtEnd(self.builder, loop_block);
            self.emit_stmt(&s.block)?;

            if !self.block_has_terminator() {
                LLVMBuildBr(self.builder, loop_block);
            }

            LLVMPositionBuilderAtEnd(self.builder, merge_block);

            self.break_label = saved_break_label;
            self.continue_label = saved_continue_label;
        }

        ok(())
    }

    fn emit_while(&mut self, s: &'ast StmtWhileType) -> EmitResult<()> {
        let cond_block = self.append_block(b"while_cond\0");
        let loop_block = self.append_block(b"while_body\0");
        let merge_block = self.append_block(b"while_merge\0");

        unsafe {
            LLVMBuildBr(self.builder, cond_block);

            LLVMPositionBuilderAtEnd(self.builder, cond_block);
            let value = self.emit_expr(&s.cond)?;
            LLVMBuildCondBr(self.builder, value, loop_block, merge_block);

            let saved_break_label = self.break_label;
            let saved_continue_label = self.continue_label;

            self.break_label = merge_block;
            self.continue_label = cond_block;

            LLVMPositionBuilderAtEnd(self.builder, loop_block);
            self.emit_stmt(&s.block)?;

            if !self.block_has_terminator() {
                LLVMBuildBr(self.builder, cond_block);
            }

            LLVMPositionBuilderAtEnd(self.builder, merge_block);

            self.break_label = saved_break_label;
            self.continue_label = saved_continue_label;
        }

        ok(())
    }

    fn emit_return(&mut self, s: &'ast StmtReturnType) -> EmitResult<()> {
        if let Some(ref expr) = s.expr {
            let value = self.emit_expr(expr)?;

            unsafe {
                LLVMBuildRet(self.builder, value);
            }

        } else {
            unsafe {
                LLVMBuildRetVoid(self.builder);
            }
        }

        ok(())
    }

    fn emit_block(&mut self, s: &'ast StmtBlockType) -> EmitResult<()> {
        for stmt in &s.stmts {
            self.emit_stmt(stmt)?;
        }

        ok(())
    }

    fn emit_var(&mut self, s: &'ast StmtVarType) -> EmitResult<()> {
        let var = *self.src.map_vars.get(s.id).unwrap();
        let ty = self.ty_var(var);
        let ty = self.llvm_ty(ty);

        let ptr = unsafe { LLVMBuildAlloca(self.builder, ty, noname()) };

        self.map_vars.insert(var, ptr);

        if let Some(ref expr) = s.expr {
            let value = self.emit_expr(expr)?;
            unsafe {
                LLVMBuildStore(self.builder, value, ptr);
            }
        }

        ok(())
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> EmitResult<LLVMValueRef> {
        match *e {
            ExprLitChar(ref lit) => self.emit_lit_char(lit),
            ExprLitInt(ref lit) => self.emit_lit_int(lit),
            ExprLitFloat(ref lit) => self.emit_lit_float(lit),
            ExprLitBool(ref lit) => self.emit_lit_bool(lit),
            ExprLitStr(_) => fail(),
            ExprLitStruct(_) => fail(),
            ExprUn(_) => fail(),
            ExprIdent(ref ident) => self.emit_ident(ident),
            ExprAssign(ref expr) => self.emit_assign(expr),
            ExprBin(ref expr) => self.emit_bin(expr),
            ExprCall(_) => fail(),
            ExprDelegation(_) => fail(),
            ExprField(_) => fail(),
            ExprSelf(_) => fail(),
            ExprSuper(_) => fail(),
            ExprNil(_) => fail(),
            ExprArray(_) => fail(),
            ExprConv(_) => fail(),
            ExprTry(_) => fail(),
            ExprLambda(_) => fail(),
        }
    }

    fn emit_lit_char(&mut self, e: &'ast ExprLitCharType) -> EmitResult<LLVMValueRef> {
        unsafe {
            let ty = LLVMInt32TypeInContext(self.context);
            let value = LLVMConstInt(ty, e.value as u64, 0);
            ok(value)
        }
    }

    fn emit_lit_int(&mut self, e: &'ast ExprLitIntType) -> EmitResult<LLVMValueRef> {
        unsafe {
            let ty = match e.suffix {
                IntSuffix::Byte => LLVMInt8TypeInContext(self.context),
                IntSuffix::Int => LLVMInt32TypeInContext(self.context),
                IntSuffix::Long => LLVMInt64TypeInContext(self.context),
            };

            let value = LLVMConstInt(ty, e.value, 0);
            ok(value)
        }
    }

    fn emit_lit_float(&mut self, e: &'ast ExprLitFloatType) -> EmitResult<LLVMValueRef> {
        unsafe {
            let ty = match e.suffix {
                FloatSuffix::Float => LLVMFloatTypeInContext(self.context),
                FloatSuffix::Double => LLVMDoubleTypeInContext(self.context),
            };

            let value = LLVMConstReal(ty, e.value);
            ok(value)
        }
    }

    fn emit_lit_bool(&mut self, e: &'ast ExprLitBoolType) -> EmitResult<LLVMValueRef> {
        ok(self.llvm_lit_bool(e.value))
    }

    fn llvm_lit_bool(&mut self, value: bool) -> LLVMValueRef {
        unsafe {
            let ty = LLVMInt1TypeInContext(self.context);
            let value = if value { 1 } else { 0 };
            let value = LLVMConstInt(ty, value, 0);

            value
        }
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType) -> EmitResult<LLVMValueRef> {
        let &ident = self.src.map_idents.get(e.id).unwrap();

        match ident {
            IdentType::Var(varid) => {
                let ptr = self.map_vars[&varid];
                let value = unsafe { LLVMBuildLoad(self.builder, ptr, noname()) };

                ok(value)
            }

            _ => fail(),
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprAssignType) -> EmitResult<LLVMValueRef> {
        if e.lhs.is_array() {
            return fail();
        }

        let &ident_type = self.src.map_idents.get(e.lhs.id()).unwrap();

        match ident_type {
            IdentType::Var(varid) => {
                let ptr = self.map_vars[&varid];
                let value = self.emit_expr(&e.rhs)?;

                unsafe {
                    LLVMBuildStore(self.builder, value, ptr);
                }

                ok(ptr::null_mut())
            }

            _ => fail(),
        }
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType) -> EmitResult<LLVMValueRef> {
        if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_bin_intrinsic(e.op, &e.lhs, &e.rhs, intrinsic)

        } else if e.op == BinOp::Or {
            self.emit_or(&e.lhs, &e.rhs)

        } else if e.op == BinOp::And {
            self.emit_and(&e.lhs, &e.rhs)

        } else if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            let lhs_value = self.emit_expr(&e.lhs)?;
            let rhs_value = self.emit_expr(&e.rhs)?;

            let predicate = if e.op == BinOp::Cmp(CmpOp::Is) {
                LLVMIntPredicate::LLVMIntEQ
            } else {
                LLVMIntPredicate::LLVMIntNE
            };

            let value =
                unsafe { LLVMBuildICmp(self.builder, predicate, lhs_value, rhs_value, noname()) };

            ok(value)

        } else {
            fail()
        }
    }

    fn emit_bin_intrinsic(
        &mut self,
        op: BinOp,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        intrinsic: Intrinsic,
    ) -> EmitResult<LLVMValueRef> {
        let lhs_value = self.emit_expr(lhs)?;
        let rhs_value = self.emit_expr(rhs)?;

        let op = match intrinsic {
            Intrinsic::IntAdd | Intrinsic::LongAdd => LLVMOpcode::LLVMAdd,
            Intrinsic::IntSub | Intrinsic::LongSub => LLVMOpcode::LLVMSub,
            Intrinsic::IntMul | Intrinsic::LongMul => LLVMOpcode::LLVMMul,
            Intrinsic::IntDiv | Intrinsic::LongDiv => LLVMOpcode::LLVMSDiv,
            Intrinsic::IntMod | Intrinsic::LongMod => LLVMOpcode::LLVMSRem,
            Intrinsic::IntShl | Intrinsic::LongShl => LLVMOpcode::LLVMShl,
            Intrinsic::IntSar | Intrinsic::LongSar => LLVMOpcode::LLVMAShr,
            Intrinsic::IntShr | Intrinsic::LongShr => LLVMOpcode::LLVMLShr,
            Intrinsic::IntAnd | Intrinsic::LongAnd => LLVMOpcode::LLVMAnd,
            Intrinsic::IntOr | Intrinsic::LongOr => LLVMOpcode::LLVMOr,
            Intrinsic::IntXor | Intrinsic::LongXor => LLVMOpcode::LLVMXor,
            Intrinsic::ByteEq | Intrinsic::BoolEq | Intrinsic::CharEq | Intrinsic::IntEq |
            Intrinsic::LongEq => {
                let predicate = if op == BinOp::Cmp(CmpOp::Eq) {
                    LLVMIntPredicate::LLVMIntEQ
                } else {
                    LLVMIntPredicate::LLVMIntNE
                };

                let value = unsafe {
                    LLVMBuildICmp(self.builder, predicate, lhs_value, rhs_value, noname())
                };

                return ok(value);
            }
            Intrinsic::IntCmp | Intrinsic::LongCmp => {
                let predicate = match op {
                    BinOp::Cmp(CmpOp::Lt) => LLVMIntPredicate::LLVMIntSLT,
                    BinOp::Cmp(CmpOp::Le) => LLVMIntPredicate::LLVMIntSLE,
                    BinOp::Cmp(CmpOp::Gt) => LLVMIntPredicate::LLVMIntSGT,
                    BinOp::Cmp(CmpOp::Ge) => LLVMIntPredicate::LLVMIntSGE,
                    _ => unreachable!(),
                };

                let value = unsafe {
                    LLVMBuildICmp(self.builder, predicate, lhs_value, rhs_value, noname())
                };

                return ok(value);
            }

            _ => {
                return fail();
            }
        };

        let value = unsafe { LLVMBuildBinOp(self.builder, op, lhs_value, rhs_value, noname()) };

        ok(value)
    }

    fn emit_or(&mut self, lhs: &'ast Expr, rhs: &'ast Expr) -> EmitResult<LLVMValueRef> {
        let true_block = self.append_block(b"or_true\0");
        let next_block = self.append_block(b"or_next\0");
        let false_block = self.append_block(b"or_false\0");
        let merge_block = self.append_block(b"or_merge\0");

        let lhs_value = self.emit_expr(lhs)?;

        unsafe {
            LLVMBuildCondBr(self.builder, lhs_value, true_block, next_block);

            LLVMPositionBuilderAtEnd(self.builder, next_block);
            let rhs_value = self.emit_expr(rhs)?;
            LLVMBuildCondBr(self.builder, rhs_value, true_block, false_block);

            LLVMPositionBuilderAtEnd(self.builder, true_block);
            LLVMBuildBr(self.builder, merge_block);

            LLVMPositionBuilderAtEnd(self.builder, false_block);
            LLVMBuildBr(self.builder, merge_block);

            LLVMPositionBuilderAtEnd(self.builder, merge_block);
            let result = LLVMBuildPhi(
                self.builder,
                self.llvm_ty(BuiltinType::Bool),
                b"and_result\0".as_ptr() as *const _,
            );

            let mut values = [self.llvm_lit_bool(true), self.llvm_lit_bool(false)];
            let mut blocks = [true_block, false_block];
            LLVMAddIncoming(result, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);

            ok(result)
        }
    }

    fn emit_and(&mut self, lhs: &'ast Expr, rhs: &'ast Expr) -> EmitResult<LLVMValueRef> {
        let true_block = self.append_block(b"and_true\0");
        let next_block = self.append_block(b"and_next\0");
        let false_block = self.append_block(b"and_false\0");
        let merge_block = self.append_block(b"and_merge\0");

        let lhs_value = self.emit_expr(lhs)?;

        unsafe {
            LLVMBuildCondBr(self.builder, lhs_value, next_block, false_block);

            LLVMPositionBuilderAtEnd(self.builder, next_block);
            let rhs_value = self.emit_expr(rhs)?;
            LLVMBuildCondBr(self.builder, rhs_value, true_block, false_block);

            LLVMPositionBuilderAtEnd(self.builder, true_block);
            LLVMBuildBr(self.builder, merge_block);

            LLVMPositionBuilderAtEnd(self.builder, false_block);
            LLVMBuildBr(self.builder, merge_block);

            LLVMPositionBuilderAtEnd(self.builder, merge_block);
            let result = LLVMBuildPhi(
                self.builder,
                self.llvm_ty(BuiltinType::Bool),
                b"and_result\0".as_ptr() as *const _,
            );

            let mut values = [self.llvm_lit_bool(true), self.llvm_lit_bool(false)];
            let mut blocks = [true_block, false_block];
            LLVMAddIncoming(result, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);

            ok(result)
        }
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn ty_var(&self, id: VarId) -> BuiltinType {
        let ty = self.src.vars[id].ty;
        self.specialize_type(ty)
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.ctxt.lists.borrow().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.ctxt.lists.borrow_mut().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }

    fn append_block(&mut self, name: &[u8]) -> LLVMBasicBlockRef {
        unsafe {
            LLVMAppendBasicBlockInContext(self.context, self.function, name.as_ptr() as *const _)
        }
    }

    fn block_has_terminator(&self) -> bool {
        unsafe {
            let block = LLVMGetInsertBlock(self.builder);
            let term = LLVMGetBasicBlockTerminator(block);

            !term.is_null()
        }
    }

    fn llvm_ty(&self, ty: BuiltinType) -> LLVMTypeRef {
        if ty.is_bool() {
            unsafe {
                return LLVMInt1TypeInContext(self.context);
            }
        }

        let mode = ty.mode();

        unsafe {
            match mode {
                MachineMode::Int64 => LLVMInt64TypeInContext(self.context),
                MachineMode::Int32 => LLVMInt32TypeInContext(self.context),
                MachineMode::Int8 => LLVMInt8TypeInContext(self.context),
                MachineMode::Float32 => LLVMFloatTypeInContext(self.context),
                MachineMode::Float64 => LLVMDoubleTypeInContext(self.context),
                MachineMode::Ptr => {
                    let int8 = LLVMInt8TypeInContext(self.context);
                    LLVMPointerType(int8, 1)
                }
            }
        }
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let call = self.src.map_calls.get(id);

        if call.is_none() {
            return None;
        }

        let fid = call.unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.ctxt.fcts[fid].borrow();

        match fct.kind {
            FctKind::Builtin(intr) => Some(intr),
            _ => None,
        }
    }
}

fn noname() -> *const i8 {
    b"\0".as_ptr() as *const _
}

extern "C" fn resolver(name: *const i8, _: *mut libc::c_void) -> u64 {
    let name = unsafe { CStr::from_ptr(name) };
    panic!("resolver unimplemented: {:?}", name);
}
