use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;

use crate::baseline::asm::BaselineAssembler;
use crate::baseline::ast::expr::*;
use crate::baseline::ast::info::JitInfo;
use crate::baseline::codegen::{
    create_gcpoint, register_for_mode, should_emit_debug, CodeGen, CondCode, ExprStore, Scopes,
    TempOffsets,
};
use crate::baseline::fct::{CatchType, Comment, JitBaselineFct, JitDescriptor};
use crate::class::{ClassDef, TypeParams};
use crate::cpu::{Mem, FREG_PARAMS, FREG_RESULT, REG_PARAMS, REG_RESULT};
use crate::ctxt::VM;
use crate::ctxt::{CallSite, Fct, FctParent, FctSrc};
use crate::masm::*;
use crate::os::signal::Trap;
use crate::semck::always_returns;
use crate::semck::specialize::specialize_class_ty;
use crate::ty::{BuiltinType, MachineMode};

pub struct AstCodeGen<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub ast: &'ast Function,
    pub asm: BaselineAssembler<'a, 'ast>,
    pub scopes: Scopes,
    pub src: &'a mut FctSrc,
    pub jit_info: JitInfo<'ast>,

    pub lbl_break: Option<Label>,
    pub lbl_continue: Option<Label>,

    // stores all active finally blocks
    pub active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    pub lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    pub active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    pub active_upper: Option<usize>,

    pub cls_type_params: &'a TypeParams,
    pub fct_type_params: &'a TypeParams,
}

impl<'a, 'ast> AstCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    fn store_register_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        if self.fct.has_self() {
            let var = self.src.var_self();
            let mode = var.ty.mode();

            self.asm.emit_comment(Comment::StoreParam(var.id));

            let dest = if mode.is_float() {
                FREG_PARAMS[0].into()
            } else {
                REG_PARAMS[0].into()
            };

            let offset = self.jit_info.offset(var.id);
            self.asm.store_mem(mode, Mem::Local(offset), dest);

            self.scopes.add_var(var.id, offset);

            if mode.is_float() {
                freg_idx += 1;
            } else {
                reg_idx += 1;
            }
        }

        for p in &self.ast.params {
            let varid = *self.src.map_vars.get(p.id).unwrap();
            let ty = self.jit_info.ty(varid);
            let is_float = ty.mode().is_float();

            if ty.reference_type() {
                let offset = self.jit_info.offset(varid);
                self.scopes.add_var(varid, offset);
            }

            if is_float && freg_idx < FREG_PARAMS.len() {
                let reg = FREG_PARAMS[freg_idx];

                self.asm.emit_comment(Comment::StoreParam(varid));
                self.asm.var_store(
                    self.jit_info.offset(varid),
                    self.jit_info.ty(varid),
                    reg.into(),
                );

                freg_idx += 1;
            } else if !is_float && reg_idx < REG_PARAMS.len() {
                let reg = REG_PARAMS[reg_idx];

                self.asm.emit_comment(Comment::StoreParam(varid));
                self.asm.var_store(
                    self.jit_info.offset(varid),
                    self.jit_info.ty(varid),
                    reg.into(),
                );

                reg_idx += 1;
            } else {
                // ignore params not stored in register
            }
        }
    }

    fn emit_prolog(&mut self) {
        let stacksize = self.jit_info.stacksize();
        self.asm.prolog(stacksize);
        self.asm.emit_comment(Comment::Lit("prolog end"));
        self.asm.emit_comment(Comment::Newline);
    }

    fn emit_epilog(&mut self) {
        self.asm.emit_comment(Comment::Newline);
        self.asm.emit_comment(Comment::Lit("epilog"));

        let stacksize = self.jit_info.stacksize();
        let polling_page = self.vm.polling_page.addr();
        self.asm.epilog_with_polling(stacksize, polling_page);
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let len = self.active_upper.unwrap_or(self.active_finallys.len());
        let return_type = self.specialize_type(self.fct.return_type);

        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);

            if len > 0 {
                let offset = self.jit_info.eh_return_value.unwrap();
                let rmode = return_type.mode();
                self.asm
                    .store_mem(rmode, Mem::Local(offset), register_for_mode(rmode));
            }
        }

        if let Some(lbl_return) = self.lbl_return {
            self.asm.jump(lbl_return);
            return;
        }

        if len > 0 {
            let mut ind = 0;
            while ind < len {
                let lbl = self.asm.create_label();
                self.lbl_return = Some(lbl);

                let finally = self.active_finallys[len - 1 - ind];
                self.visit_stmt(finally);

                self.asm.bind_label(lbl);

                ind += 1;
            }

            if s.expr.is_some() {
                let offset = self.jit_info.eh_return_value.unwrap();
                let rmode = return_type.mode();
                self.asm
                    .load_mem(rmode, register_for_mode(rmode), Mem::Local(offset));
            }

            self.lbl_return = None;
        }

        self.emit_epilog();
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.asm.bind_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation
        } else {
            // execute condition, when condition is false jump to
            // end of while
            self.emit_expr(&s.cond);
            self.asm
                .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_end);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.asm.jump(lbl_start);
        });

        self.asm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_stmt_for(&mut self, s: &'ast StmtForType) {
        let for_info = self.jit_info.map_fors.get(s.id).unwrap().clone();

        // emit: <iterator> = obj.makeIterator()
        let dest = self.emit_call_site(&for_info.make_iterator, s.pos);

        // offset of iterator storage
        let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Local(offset), dest);

        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.asm.bind_label(lbl_start);

        // emit: iterator.hasNext() & jump to lbl_end if false
        let dest = self.emit_call_site(&for_info.has_next, s.pos);
        self.asm
            .test_and_jump_if(CondCode::Zero, dest.reg(), lbl_end);

        // emit: <for_var> = iterator.next()
        let dest = self.emit_call_site(&for_info.next, s.pos);

        let for_var_id = *self.src.map_vars.get(s.id).unwrap();
        self.asm.var_store(
            self.jit_info.offset(for_var_id),
            self.jit_info.ty(for_var_id),
            dest,
        );

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.asm.jump(lbl_start);
        });

        self.asm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.asm.bind_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.asm.jump(lbl_start);
        });

        self.asm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_safepoint(&mut self) {
        self.asm.emit_comment(Comment::ReadPollingPage);
        self.asm.check_polling_page(self.vm.polling_page.addr());

        let temps = TempOffsets::new();
        let gcpoint = create_gcpoint(&self.scopes, &temps);
        self.asm.emit_gcpoint(gcpoint);
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
    where
        F: FnOnce(&mut AstCodeGen<'a, 'ast>),
    {
        let old_lbl_break = self.lbl_break;
        let old_lbl_continue = self.lbl_continue;

        self.lbl_break = Some(lbl_break);
        self.lbl_continue = Some(lbl_continue);

        f(self);

        self.lbl_break = old_lbl_break;
        self.lbl_continue = old_lbl_continue;
    }

    fn emit_stmt_if(&mut self, s: &'ast StmtIfType) {
        let lbl_end = self.asm.create_label();
        let lbl_else = if let Some(_) = s.else_block {
            self.asm.create_label()
        } else {
            lbl_end
        };

        self.emit_expr(&s.cond);
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            self.asm.jump(lbl_end);
            self.asm.bind_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.asm.bind_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType) {
        // emit finallys between loop and break
        self.emit_finallys_within_loop();

        // now jump out of loop
        let lbl_break = self.lbl_break.unwrap();
        self.asm.jump(lbl_break);
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        // emit finallys between loop and continue
        self.emit_finallys_within_loop();

        // now jump to start of loop
        let lbl_continue = self.lbl_continue.unwrap();
        self.asm.jump(lbl_continue);
    }

    fn emit_finallys_within_loop(&mut self) {
        let finallys_len = self.active_upper.unwrap_or(self.active_finallys.len());
        let start = self.active_loop.unwrap_or(0);

        if finallys_len == 0 || start >= finallys_len {
            return;
        }

        let mut ind = 0;
        let end = finallys_len - start;

        let saved_active_upper = self.active_upper;

        while ind < end {
            let idx = finallys_len - 1 - ind;
            self.active_upper = Some(idx);

            let finally = self.active_finallys[idx];
            self.visit_stmt(finally);

            ind += 1;
        }

        self.active_upper = saved_active_upper;
    }

    fn emit_stmt_expr(&mut self, s: &'ast StmtExprType) {
        self.emit_expr(&s.expr);
    }

    fn emit_stmt_block(&mut self, s: &'ast StmtBlockType) {
        self.scopes.push_scope();

        for stmt in &s.stmts {
            self.visit_stmt(stmt);
        }

        self.scopes.pop_scope();
    }

    fn emit_stmt_var(&mut self, s: &'ast StmtVarType) {
        let mut initialized = false;
        let var = *self.src.map_vars.get(s.id).unwrap();

        if let Some(ref expr) = s.expr {
            let value = self.emit_expr(expr);
            initialized = true;

            self.asm
                .var_store(self.jit_info.offset(var), self.jit_info.ty(var), value);
        }

        let reference_type = {
            let ty = self.jit_info.ty(var);

            if ty.reference_type() {
                let offset = self.jit_info.offset(var);
                self.scopes.add_var(var, offset);
            }

            ty.reference_type()
        };

        // uninitialized variables which reference objects need to be initialized to null
        // otherwise the GC  can't know if the stored value is a valid pointer
        if reference_type && !initialized {
            self.asm.load_nil(REG_RESULT);
            self.asm.var_store(
                self.jit_info.offset(var),
                self.jit_info.ty(var),
                REG_RESULT.into(),
            );
        }
    }

    fn emit_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        self.emit_expr(&s.expr);
        self.asm.test_if_nil_bailout(s.pos, REG_RESULT, Trap::NIL);

        self.asm.throw(REG_RESULT, s.pos);
    }

    fn emit_stmt_do(&mut self, s: &'ast StmtDoType) {
        let lbl_after = self.asm.create_label();

        let do_span = self.stmt_with_finally(s, &s.do_block, lbl_after);
        let catch_spans = self.emit_do_catch_blocks(s, do_span, lbl_after);
        let finally_start = self.emit_do_finally_block(s);

        self.asm.bind_label(lbl_after);

        if let Some(finally_start) = finally_start {
            let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
            self.asm
                .emit_exception_handler(do_span, finally_start, Some(offset), CatchType::Any);

            for &catch_span in &catch_spans {
                self.asm.emit_exception_handler(
                    catch_span,
                    finally_start,
                    Some(offset),
                    CatchType::Any,
                );
            }
        }
    }

    fn emit_do_catch_blocks(
        &mut self,
        s: &'ast StmtDoType,
        try_span: (usize, usize),
        lbl_after: Label,
    ) -> Vec<(usize, usize)> {
        let mut ret = Vec::new();

        for catch in &s.catch_blocks {
            let varid = *self.src.map_vars.get(catch.id).unwrap();
            let offset = self.jit_info.offset(varid);

            self.scopes.push_scope();
            self.scopes.add_var(varid, offset);

            let catch_span = self.stmt_with_finally(s, &catch.block, lbl_after);

            self.scopes.pop_scope();

            let ty = self.src.ty(catch.data_type.id());
            let ty = self.specialize_type(ty);
            let cls_def_id = specialize_class_ty(self.vm, ty);
            let cls_def = self.vm.class_defs.idx(cls_def_id);
            let cls_def = cls_def.read();

            let catch_type = CatchType::Class(&*cls_def as *const ClassDef);
            self.asm
                .emit_exception_handler(try_span, catch_span.0, Some(offset), catch_type);

            ret.push(catch_span);
        }

        ret
    }

    fn stmt_with_finally(
        &mut self,
        s: &'ast StmtDoType,
        stmt: &'ast Stmt,
        lbl_after: Label,
    ) -> (usize, usize) {
        if s.finally_block.is_some() {
            let finally = &*s.finally_block.as_ref().unwrap().block;
            self.active_finallys.push(finally);
        }

        let start = self.asm.pos();
        self.visit_stmt(stmt);
        let end = self.asm.pos();

        if s.finally_block.is_some() {
            self.active_finallys.pop();
        }

        if !always_returns(stmt) {
            if let Some(ref finally_block) = s.finally_block {
                self.visit_stmt(&finally_block.block);
            }

            self.asm.jump(lbl_after);
        }

        (start, end)
    }

    fn emit_do_finally_block(&mut self, s: &'ast StmtDoType) -> Option<usize> {
        if s.finally_block.is_none() {
            return None;
        }
        let finally_block = s.finally_block.as_ref().unwrap();

        let finally_pos = self.asm.pos();

        self.scopes.push_scope();

        let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
        self.scopes.add_var_offset(offset);

        self.visit_stmt(&finally_block.block);

        self.asm
            .load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Local(offset));
        self.asm.throw(REG_RESULT, s.pos);

        self.scopes.pop_scope();

        Some(finally_pos)
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> ExprStore {
        let ty = self
            .src
            .map_tys
            .get(e.id())
            .map(|ty| *ty)
            .unwrap_or(BuiltinType::Int);

        let ty = self.specialize_type(ty);

        let dest: ExprStore = if ty.is_float() {
            FREG_RESULT.into()
        } else {
            REG_RESULT.into()
        };

        let expr_gen = ExprGen::new(
            self.vm,
            self.fct,
            self.src,
            self.ast,
            &mut self.asm,
            &mut self.scopes,
            &self.jit_info,
            self.cls_type_params,
            self.fct_type_params,
        );

        expr_gen.generate(e, dest);

        dest
    }

    fn emit_call_site(&mut self, call_site: &CallSite<'ast>, pos: Position) -> ExprStore {
        let callee = self.vm.fcts.idx(call_site.callee);
        let callee = callee.read();
        let return_type = self.specialize_type(callee.return_type);

        let dest = register_for_mode(return_type.mode());

        let mut expr_gen = ExprGen::new(
            self.vm,
            self.fct,
            self.src,
            self.ast,
            &mut self.asm,
            &mut self.scopes,
            &self.jit_info,
            self.cls_type_params,
            self.fct_type_params,
        );

        expr_gen.emit_call_site(call_site, pos, dest);

        dest
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                debug_assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                debug_assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.vm.lists.lock().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.vm.lists.lock().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }
}

impl<'a, 'ast> CodeGen<'ast> for AstCodeGen<'a, 'ast> {
    fn generate(mut self) -> JitBaselineFct {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.emit_prolog();
        self.store_register_params_on_stack();
        self.visit_fct(self.ast);

        let always_returns = self.src.always_returns;

        if !always_returns {
            self.emit_epilog();
        }

        let jit_fct = self.asm.jit(
            self.jit_info.stacksize(),
            JitDescriptor::DoraFct(self.fct.id),
            self.ast.throws,
        );

        jit_fct
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for AstCodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.emit_stmt_expr(stmt),
            StmtIf(ref stmt) => self.emit_stmt_if(stmt),
            StmtLoop(ref stmt) => self.emit_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.emit_stmt_while(stmt),
            StmtFor(ref stmt) => self.emit_stmt_for(stmt),
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            StmtBreak(ref stmt) => self.emit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.emit_stmt_continue(stmt),
            StmtBlock(ref stmt) => self.emit_stmt_block(stmt),
            StmtVar(ref stmt) => self.emit_stmt_var(stmt),
            StmtThrow(ref stmt) => self.emit_stmt_throw(stmt),
            StmtDefer(_) => unimplemented!(),
            StmtDo(ref stmt) => self.emit_stmt_do(stmt),
            StmtSpawn(_) => unimplemented!(),
        }
    }

    fn visit_expr(&mut self, _: &'ast Expr) {
        unreachable!("should not be invoked");
    }
}
