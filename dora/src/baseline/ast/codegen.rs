use dora_parser::ast::visit::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use crate::baseline::asm::BaselineAssembler;
use crate::baseline::ast::info::JitInfo;
use crate::baseline::codegen::{
    create_gcpoint, ensure_native_stub, register_for_mode, should_emit_debug, AllocationSize,
    CodeGen, CondCode, ExprStore, Scopes, TempOffsets,
};
use crate::baseline::dora_native::{InternalFct, InternalFctDescriptor};
use crate::baseline::fct::{CatchType, Comment, JitBaselineFct, JitDescriptor};
use crate::class::{ClassDef, ClassDefId, ClassSize, FieldId};
use crate::cpu::{
    FReg, Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_TMP1, REG_TMP2,
};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::object::{Header, Str};
use crate::os::signal::Trap;
use crate::semck::always_returns;
use crate::semck::specialize::{specialize_class_id, specialize_class_ty};
use crate::ty::{BuiltinType, MachineMode, TypeList};
use crate::vm::{
    Arg, CallSite, ConstId, Fct, FctId, FctKind, FctParent, FctSrc, IdentType, Intrinsic, VM,
};
use crate::vtable::{VTable, DISPLAY_SIZE};

pub struct AstCodeGen<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub ast: &'ast Function,
    pub asm: BaselineAssembler<'a, 'ast>,
    pub scopes: Scopes,
    pub src: &'a mut FctSrc,
    pub jit_info: &'a JitInfo<'ast>,

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

    pub temps: TempOffsets,

    pub cls_type_params: &'a TypeList,
    pub fct_type_params: &'a TypeList,
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
        self.asm.prolog(stacksize, self.fct.ast.pos);
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
            self.emit_expr_old(expr);

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
            self.emit_expr_old(&s.cond);
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
        let dest = self.emit_call_site_old(&for_info.make_iterator, s.pos);

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
        let dest = self.emit_call_site_old(&for_info.has_next, s.pos);
        self.asm
            .test_and_jump_if(CondCode::Zero, dest.reg(), lbl_end);

        // emit: <for_var> = iterator.next()
        let dest = self.emit_call_site_old(&for_info.next, s.pos);

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

        self.emit_expr_old(&s.cond);
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
        self.emit_expr_old(&s.expr);
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
            let value = self.emit_expr_old(expr);
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
        self.emit_expr_old(&s.expr);
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

    fn emit_expr_old(&mut self, e: &'ast Expr) -> ExprStore {
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

        self.emit_expr(e, dest);

        dest
    }

    fn emit_call_site_old(&mut self, call_site: &CallSite<'ast>, pos: Position) -> ExprStore {
        let callee = self.vm.fcts.idx(call_site.callee);
        let callee = callee.read();
        let return_type = self.specialize_type(callee.return_type);

        let dest = register_for_mode(return_type.mode());

        self.emit_call_site(call_site, pos, dest);

        dest
    }

    fn emit_expr(&mut self, e: &'ast Expr, dest: ExprStore) {
        match *e {
            ExprLitChar(ref expr) => self.emit_lit_char(expr, dest.reg()),
            ExprLitInt(ref expr) => self.emit_lit_int(expr, dest.reg()),
            ExprLitFloat(ref expr) => self.emit_lit_float(expr, dest.freg()),
            ExprLitBool(ref expr) => self.emit_lit_bool(expr, dest.reg()),
            ExprLitStr(ref expr) => self.emit_lit_str(expr, dest.reg()),
            ExprUn(ref expr) => self.emit_unary_operator(expr, dest),
            ExprIdent(ref expr) => self.emit_ident(expr, dest),
            ExprBin(ref expr) => self.emit_bin(expr, dest),
            ExprCall(ref expr) => self.emit_call(expr, dest),
            ExprTypeParam(_) => unreachable!(),
            ExprPath(ref expr) => self.emit_path(expr, dest),
            ExprDelegation(ref expr) => self.emit_delegation(expr, dest),
            ExprDot(ref expr) => self.emit_dot(expr, dest),
            ExprSelf(_) => self.emit_self(dest),
            ExprSuper(_) => self.emit_self(dest),
            ExprNil(_) => self.emit_nil(dest.reg()),
            ExprConv(ref expr) => self.emit_conv(expr, dest.reg()),
            ExprTemplate(ref expr) => self.emit_template(expr, dest.reg()),
            ExprTry(ref expr) => self.emit_try(expr, dest),
            ExprLambda(_) => unimplemented!(),
            ExprBlock(ref expr) => self.emit_block(expr, dest),
        }
    }

    fn emit_block(&mut self, block: &'ast ExprBlockType, dest: ExprStore) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref expr) = block.expr {
            self.emit_expr(expr, dest);
        }
    }

    fn emit_try(&mut self, e: &'ast ExprTryType, dest: ExprStore) {
        match e.mode {
            TryMode::Normal => {
                self.emit_expr(&e.expr, dest);
            }

            TryMode::Else(ref alt_expr) => {
                let lbl_after = self.asm.create_label();

                let try_span = {
                    let start = self.asm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.asm.pos();

                    self.asm.jump(lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.asm.pos();
                    self.emit_expr(alt_expr, dest);
                    let end = self.asm.pos();

                    (start, end)
                };

                self.asm
                    .emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.asm.bind_label(lbl_after);
            }

            TryMode::Force => {
                let lbl_after = self.asm.create_label();

                let try_span = {
                    let start = self.asm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.asm.pos();

                    self.asm.jump(lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.asm.pos();
                    self.asm.emit_bailout_inplace(Trap::UNEXPECTED, e.pos);
                    let end = self.asm.pos();

                    (start, end)
                };

                self.asm
                    .emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.asm.bind_label(lbl_after);
            }

            TryMode::Opt => panic!("unsupported"),
        }
    }

    fn emit_template(&mut self, e: &'ast ExprTemplateType, dest: Reg) {
        let template_info = self
            .jit_info
            .map_templates
            .get(e.id)
            .expect("no TemplateJitInfo found");
        self.emit_call_site(&template_info.string_buffer_new, e.pos, REG_RESULT.into());
        self.asm.var_store(
            template_info.string_buffer_offset,
            BuiltinType::Ptr,
            REG_RESULT.into(),
        );

        self.add_temp(template_info.string_buffer_offset, BuiltinType::Ptr);

        for (idx, part) in e.parts.iter().enumerate() {
            let part_info = &template_info.part_infos[idx];

            if let Some(ref lit_str) = part.to_lit_str() {
                self.emit_lit_str_value(&lit_str.value, REG_RESULT);
            } else {
                let ty = self.ty(part.id());

                let dest = result_reg(ty.mode());
                self.emit_expr(part, dest);

                if ty.cls_id(self.vm) != Some(self.vm.vips.string_class) {
                    self.asm.var_store(
                        part_info.object_offset.expect("object_info missing"),
                        ty,
                        dest,
                    );

                    let to_string = part_info.to_string.as_ref().unwrap();
                    self.emit_call_site(to_string, e.pos, REG_RESULT.into());
                }
            }

            self.asm.var_store(
                template_info.string_part_offset,
                BuiltinType::Ptr,
                REG_RESULT.into(),
            );

            self.emit_call_site(&part_info.append, e.pos, dest.into());
        }

        self.emit_call_site(&template_info.string_buffer_to_string, e.pos, dest.into());
        self.free_temp(template_info.string_buffer_offset, BuiltinType::Ptr);
    }

    fn emit_conv(&mut self, e: &'ast ExprConvType, dest: Reg) {
        self.emit_expr(&e.object, dest.into());

        // return false if object is nil
        let lbl_nil = self.asm.test_if_nil(dest);
        let conv = *self.src.map_convs.get(e.id).unwrap();

        if conv.valid {
            if e.is {
                // return true for object is T
                self.asm.load_true(dest);
            } else {
                // do nothing for object as T
            }
        } else {
            let cls_id = conv.cls_id;
            let cls_id = specialize_class_id(self.vm, cls_id);
            let cls = self.vm.class_defs.idx(cls_id);
            let cls = cls.read();

            let vtable: &VTable = cls.vtable.as_ref().unwrap();

            let offset = if e.is {
                0
            } else {
                // reserve temp variable for object
                let offset = self.reserve_temp_for_node(&e.object);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(offset), dest.into());

                offset
            };

            // object instanceof T

            // tmp1 = <vtable of object>
            self.asm
                .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Base(dest, 0));

            let disp = self.asm.add_addr(vtable as *const _ as *mut u8);
            let pos = self.asm.pos() as i32;

            // tmp2 = <vtable of T>
            self.asm.load_constpool(REG_TMP2, disp + pos);

            if vtable.subtype_depth >= DISPLAY_SIZE as i32 {
                // cmp [tmp1 + offset T.vtable.subtype_depth], tmp3
                self.asm.cmp_mem_imm(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, VTable::offset_of_depth()),
                    vtable.subtype_depth,
                );

                // jnz lbl_false
                let lbl_false = self.asm.create_label();
                self.asm.jump_if(CondCode::Less, lbl_false);

                // tmp1 = tmp1.subtype_overflow
                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Base(REG_TMP1, VTable::offset_of_overflow()),
                );

                let overflow_offset =
                    mem::ptr_width() * (vtable.subtype_depth - DISPLAY_SIZE as i32);

                // cmp [tmp1 + 8*(vtable.subtype_depth - DISPLAY_SIZE) ], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, overflow_offset),
                    REG_TMP2,
                );

                if e.is {
                    // dest = if zero then true else false
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    // jump to lbl_false if cmp did not succeed
                    self.asm.jump_if(CondCode::NonZero, lbl_false);

                    // otherwise load temp variable again
                    self.asm
                        .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
                }

                // jmp lbl_finished
                let lbl_finished = self.asm.create_label();
                self.asm.jump(lbl_finished);

                // lbl_false:
                self.asm.bind_label(lbl_false);

                if e.is {
                    // dest = false
                    self.asm.load_false(dest);
                } else {
                    // bailout
                    self.asm.emit_bailout_inplace(Trap::CAST, e.pos);
                }

                // lbl_finished:
                self.asm.bind_label(lbl_finished);
            } else {
                let display_entry =
                    VTable::offset_of_display() + vtable.subtype_depth * mem::ptr_width();

                // tmp1 = vtable of object
                // tmp2 = vtable of T
                // cmp [tmp1 + offset], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, display_entry),
                    REG_TMP2,
                );

                if e.is {
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    let lbl_bailout = self.asm.create_label();
                    self.asm.jump_if(CondCode::NotEqual, lbl_bailout);
                    self.asm.emit_bailout(lbl_bailout, Trap::CAST, e.pos);

                    self.asm
                        .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
                }
            }

            if !e.is {
                self.free_temp_for_node(&e.object, offset);
            }
        }

        // lbl_nil:
        self.asm.bind_label(lbl_nil);

        // for is we are finished: dest is null which is boolean false
        // also for as we are finished: dest is null and stays null
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        let id = expr.id();
        let ty = self.ty(id);
        let offset = -(self.jit_info.localsize + self.jit_info.get_store(id).offset());

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn add_temp(&mut self, offset: i32, ty: BuiltinType) {
        if ty.reference_type() {
            self.temps.insert(offset);
        }
    }

    fn free_temp(&mut self, offset: i32, ty: BuiltinType) {
        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn reserve_temp_for_arg(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.jit_info.localsize + arg.offset());
        let ty = arg.ty();

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn reserve_temp_for_self(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.jit_info.localsize + arg.offset());

        offset
    }

    fn free_temp_for_node(&mut self, expr: &Expr, offset: i32) {
        let ty = self.ty(expr.id());

        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn free_temp_with_type(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        self.jit_info.map_intrinsics.get(id).map(|&intr| intr)
    }

    fn emit_self(&mut self, dest: ExprStore) {
        let var = self.src.var_self();

        self.asm.emit_comment(Comment::LoadSelf(var.id));

        let offset = self.jit_info.offset(var.id);
        self.asm
            .load_mem(var.ty.mode(), dest.into(), Mem::Local(offset));
    }

    fn emit_nil(&mut self, dest: Reg) {
        self.asm.load_nil(dest);
    }

    fn emit_dot(&mut self, expr: &'ast ExprDotType, dest: ExprStore) {
        let (ty, field) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

            match ident_type {
                &IdentType::Field(ty, field) => (ty, field),
                _ => unreachable!(),
            }
        };

        let ty = self.specialize_type(ty);

        self.emit_expr(&expr.object, REG_RESULT.into());
        self.emit_field_access(expr.pos, ty, field, REG_RESULT, dest);
    }

    fn emit_field_access(
        &mut self,
        pos: Position,
        ty: BuiltinType,
        fieldid: FieldId,
        src: Reg,
        dest: ExprStore,
    ) {
        let cls_id = specialize_class_ty(self.vm, ty);
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let field = &cls.fields[fieldid.idx()];

        self.asm.emit_comment(Comment::LoadField(cls_id, fieldid));
        self.asm
            .load_field(field.ty.mode(), dest, src, field.offset, pos.line as i32);
    }

    fn emit_lit_char(&mut self, lit: &'ast ExprLitCharType, dest: Reg) {
        self.asm
            .load_int_const(MachineMode::Int32, dest, lit.value as i64);
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg) {
        let ty = match lit.suffix {
            IntSuffix::Byte => MachineMode::Int8,
            IntSuffix::Int => MachineMode::Int32,
            IntSuffix::Long => MachineMode::Int64,
        };

        self.asm.load_int_const(ty, dest, lit.value as i64);
    }

    fn emit_lit_float(&mut self, lit: &'ast ExprLitFloatType, dest: FReg) {
        let ty = match lit.suffix {
            FloatSuffix::Float => MachineMode::Float32,
            FloatSuffix::Double => MachineMode::Float64,
        };

        self.asm.load_float_const(ty, dest, lit.value);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        if lit.value {
            self.asm.load_true(dest);
        } else {
            self.asm.load_false(dest);
        };
    }

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        self.emit_lit_str_value(&lit.value, dest);
    }

    fn emit_lit_str_value(&mut self, lit_value: &str, dest: Reg) {
        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());

        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadString(handle));
        self.asm.load_constpool(dest, disp + pos);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: ExprStore) {
        let ident = self.src.map_idents.get(e.id).unwrap();

        match ident {
            &IdentType::Var(varid) => {
                self.asm.emit_comment(Comment::LoadVar(varid));
                self.asm
                    .var_load(self.jit_info.offset(varid), self.jit_info.ty(varid), dest)
            }

            &IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let glob = glob.lock();

                let disp = self.asm.add_addr(glob.address_value.to_ptr());
                let pos = self.asm.pos() as i32;

                self.asm.emit_comment(Comment::LoadGlobal(gid));
                self.asm.load_constpool(REG_TMP1, disp + pos);

                self.asm
                    .load_mem(glob.ty.mode(), dest, Mem::Base(REG_TMP1, 0));
            }

            &IdentType::Field(cls, field) => {
                self.emit_self(REG_RESULT.into());
                self.emit_field_access(e.pos, cls, field, REG_RESULT, dest);
            }

            &IdentType::Struct(_) => {
                unimplemented!();
            }

            &IdentType::Const(const_id) => {
                self.emit_const(const_id, dest);
            }

            &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn emit_const(&mut self, const_id: ConstId, dest: ExprStore) {
        let xconst = self.vm.consts.idx(const_id);
        let xconst = xconst.lock();
        let ty = xconst.ty;

        match ty {
            BuiltinType::Bool => {
                if xconst.value.to_bool() {
                    self.asm.load_true(dest.reg());
                } else {
                    self.asm.load_false(dest.reg());
                }
            }

            BuiltinType::Char => {
                self.asm.load_int_const(
                    MachineMode::Int32,
                    dest.reg(),
                    xconst.value.to_char() as i64,
                );
            }

            BuiltinType::Byte | BuiltinType::Int | BuiltinType::Long => {
                self.asm
                    .load_int_const(ty.mode(), dest.reg(), xconst.value.to_int());
            }

            BuiltinType::Float | BuiltinType::Double => {
                self.asm
                    .load_float_const(ty.mode(), dest.freg(), xconst.value.to_float());
            }

            _ => unimplemented!(),
        }
    }

    fn emit_intrinsic_unary(&mut self, e: &'ast Expr, dest: ExprStore, intrinsic: Intrinsic) {
        self.emit_expr(&e, dest);

        match intrinsic {
            Intrinsic::IntPlus
            | Intrinsic::LongPlus
            | Intrinsic::FloatPlus
            | Intrinsic::DoublePlus => {}

            Intrinsic::IntNeg | Intrinsic::LongNeg => {
                let dest = dest.reg();

                let mode = if intrinsic == Intrinsic::IntNeg {
                    MachineMode::Int32
                } else {
                    MachineMode::Int64
                };

                self.asm.int_neg(mode, dest, dest);
            }

            Intrinsic::FloatNeg | Intrinsic::DoubleNeg => {
                let dest = dest.freg();

                let mode = if intrinsic == Intrinsic::FloatNeg {
                    MachineMode::Float32
                } else {
                    MachineMode::Float64
                };

                self.asm.float_neg(mode, dest, dest);
            }

            Intrinsic::ByteNot => {
                let dest = dest.reg();
                self.asm.int_not(MachineMode::Int8, dest, dest)
            }

            Intrinsic::IntNot | Intrinsic::LongNot => {
                let dest = dest.reg();

                let mode = if intrinsic == Intrinsic::IntNot {
                    MachineMode::Int32
                } else {
                    MachineMode::Int64
                };

                self.asm.int_not(mode, dest, dest);
            }

            Intrinsic::BoolNot => {
                let dest = dest.reg();
                self.asm.bool_not(dest, dest)
            }

            _ => panic!("unexpected intrinsic {:?}", intrinsic),
        }
    }

    fn emit_unary_operator(&mut self, e: &'ast ExprUnType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            self.emit_intrinsic_unary(&e.opnd, dest, intrinsic);
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprBinType) {
        let call_type = self.src.map_calls.get(e.id);

        if call_type.is_some() {
            let call_expr = e.lhs.to_call().unwrap();

            if let Some(intrinsic) = self.intrinsic(e.id) {
                let object = &call_expr.callee;
                let index = &call_expr.args[0];
                let value = &e.rhs;

                match intrinsic {
                    Intrinsic::GenericArraySet => {
                        let element_type = self.ty(object.id()).type_params(self.vm)[0];
                        self.emit_array_set(
                            e.pos,
                            element_type,
                            element_type.mode(),
                            object,
                            index,
                            value,
                        )
                    }

                    Intrinsic::StrSet => self.emit_array_set(
                        e.pos,
                        BuiltinType::Byte,
                        MachineMode::Int8,
                        object,
                        index,
                        value,
                    ),

                    _ => panic!("unexpected intrinsic {:?}", intrinsic),
                }
            } else {
                self.emit_call_site_id(e.id, e.pos, REG_RESULT.into());
            }

            return;
        }

        let ident_type = self.src.map_idents.get(e.lhs.id()).unwrap();

        match ident_type {
            &IdentType::Var(varid) => {
                let ty = self.jit_info.ty(varid);
                let dest = result_reg(ty.mode());
                self.emit_expr(&e.rhs, dest);

                self.asm.emit_comment(Comment::StoreVar(varid));
                self.asm
                    .var_store(self.jit_info.offset(varid), self.jit_info.ty(varid), dest);
            }

            &IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let (address_value, ty) = {
                    let glob = glob.lock();
                    (glob.address_value, glob.ty)
                };

                let dest = result_reg(ty.mode());
                self.emit_expr(&e.rhs, dest);

                let disp = self.asm.add_addr(address_value.to_ptr());
                let pos = self.asm.pos() as i32;

                self.asm.emit_comment(Comment::StoreGlobal(gid));
                self.asm.load_constpool(REG_TMP1, disp + pos);

                self.asm.store_mem(ty.mode(), Mem::Base(REG_TMP1, 0), dest);
            }

            &IdentType::Field(ty, fieldid) => {
                let ty = self.specialize_type(ty);
                let cls_id = specialize_class_ty(self.vm, ty);
                let cls = self.vm.class_defs.idx(cls_id);
                let cls = cls.read();
                let field = &cls.fields[fieldid.idx()];

                let temp = if let Some(expr_field) = e.lhs.to_dot() {
                    self.emit_expr(&expr_field.object, REG_RESULT.into());

                    &expr_field.object
                } else {
                    self.emit_self(REG_RESULT.into());

                    &e.lhs
                };

                let temp_offset = self.reserve_temp_for_node(temp);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(temp_offset), REG_RESULT.into());

                let reg = result_reg(field.ty.mode());
                let verify_refs = self.vm.args.flag_gc_verify_write && field.ty.reference_type();
                let temp_value_offset = if verify_refs {
                    self.reserve_temp_for_node(&e.rhs)
                } else {
                    0
                };
                self.emit_expr(&e.rhs, reg);
                if verify_refs {
                    self.asm
                        .store_mem(field.ty.mode(), Mem::Local(temp_value_offset), reg);
                }
                self.asm
                    .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(temp_offset));

                self.asm.emit_comment(Comment::StoreField(cls_id, fieldid));

                let write_barrier = self.vm.gc.needs_write_barrier() && field.ty.reference_type();
                let card_table_offset = self.vm.gc.card_table_offset();

                self.asm.store_field(
                    field.ty.mode(),
                    REG_TMP1,
                    field.offset,
                    reg,
                    e.pos.line as i32,
                    write_barrier,
                    card_table_offset,
                );

                if verify_refs {
                    let gcpoint = create_gcpoint(&self.scopes, &self.temps);
                    self.asm.load_mem(
                        MachineMode::Ptr,
                        REG_PARAMS[0].into(),
                        Mem::Local(temp_offset),
                    );
                    self.asm.load_mem(
                        MachineMode::Ptr,
                        REG_PARAMS[1].into(),
                        Mem::Local(temp_value_offset),
                    );
                    self.asm
                        .verify_refs(REG_PARAMS[0], REG_PARAMS[1], e.pos, gcpoint);
                    self.free_temp_for_node(&e.rhs, temp_value_offset);
                }

                self.free_temp_for_node(temp, temp_offset);
            }

            &IdentType::Struct(_) => {
                unimplemented!();
            }

            &IdentType::Const(_) | &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => {
                unreachable!();
            }

            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: ExprStore) {
        if e.op.is_any_assign() {
            self.emit_assign(e);
        } else if let Some(intrinsic) = self.intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, dest, intrinsic, Some(e.op));
        } else if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e, dest.reg());
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e, dest.reg());
        } else if e.op == BinOp::And {
            self.emit_bin_and(e, dest.reg());
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);

            match e.op {
                BinOp::Cmp(CmpOp::Eq) => {}
                BinOp::Cmp(CmpOp::Ne) => {
                    let dest = dest.reg();
                    self.asm.bool_not(dest, dest);
                }

                BinOp::Cmp(op) => {
                    let dest = dest.reg();

                    let temp = if dest == REG_RESULT {
                        REG_TMP1
                    } else {
                        REG_RESULT
                    };

                    self.asm.load_int_const(MachineMode::Int32, temp, 0);
                    self.asm.cmp_reg(MachineMode::Int32, dest, temp);
                    self.asm.set(dest, to_cond_code(op));
                }
                _ => {}
            }
        }
    }

    fn emit_bin_is(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let builtin_type = self.ty(e.lhs.id());
        let dest_mode = match builtin_type {
            BuiltinType::Nil => MachineMode::Ptr,
            BuiltinType::Float => MachineMode::Int32,
            BuiltinType::Double => MachineMode::Int64,
            _ => builtin_type.mode(),
        };
        let offset;
        if builtin_type.is_float() {
            let src_mode = builtin_type.mode();

            self.emit_expr(&e.lhs, FREG_RESULT.into());
            self.asm
                .float_as_int(dest_mode, REG_RESULT, src_mode, FREG_RESULT);
            offset = self.reserve_temp_for_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(offset), REG_RESULT.into());

            self.emit_expr(&e.rhs, FREG_RESULT.into());
            self.asm
                .float_as_int(dest_mode, REG_TMP1, src_mode, FREG_RESULT);
        } else {
            self.emit_expr(&e.lhs, REG_RESULT.into());
            offset = self.reserve_temp_for_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(offset), REG_RESULT.into());

            self.emit_expr(&e.rhs, REG_TMP1.into());
        }

        self.asm
            .load_mem(dest_mode, REG_RESULT.into(), Mem::Local(offset));
        self.asm.cmp_reg(dest_mode, REG_RESULT, REG_TMP1);

        let op = match e.op {
            BinOp::Cmp(CmpOp::Is) => CondCode::Equal,
            _ => CondCode::NotEqual,
        };

        self.asm.set(dest, op);
        self.free_temp_for_node(&e.lhs, offset);
    }

    fn emit_bin_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::NonZero, REG_RESULT, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn ptr_for_fct_id(
        &mut self,
        fid: FctId,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
    ) -> Address {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.src, self.vm, cls_type_params, fct_type_params)
        } else {
            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();

            match fct.kind {
                FctKind::Source(_) => {
                    let src = fct.src();
                    let mut src = src.write();

                    ensure_jit_or_stub_ptr(&mut src, self.vm, cls_type_params, fct_type_params)
                }

                FctKind::Native(ptr) => {
                    let internal_fct = InternalFct {
                        ptr,
                        args: fct.params_with_self(),
                        return_type: fct.return_type,
                        throws: fct.ast.throws,
                        desc: InternalFctDescriptor::NativeThunk(fid),
                    };

                    ensure_native_stub(self.vm, fid, internal_fct)
                }

                FctKind::Definition => panic!("prototype for fct call"),
                FctKind::Builtin(_) => panic!("intrinsic fct call"),
            }
        }
    }

    fn emit_path(&mut self, e: &'ast ExprPathType, dest: ExprStore) {
        let ident_type = self.src.map_idents.get(e.id).unwrap();

        match ident_type {
            &IdentType::EnumValue(_, value) => {
                self.asm
                    .load_int_const(MachineMode::Int32, dest.reg(), value as i64);
            }

            _ => unreachable!(),
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            let mut args: Vec<&'ast Expr> = Vec::with_capacity(3);
            let call_type = self.src.map_calls.get(e.id).unwrap();

            if call_type.is_expr() {
                args.push(&e.callee);
            } else if call_type.is_method() {
                args.push(e.object().unwrap());
            }

            for arg in &e.args {
                args.push(arg);
            }

            self.emit_call_intrinsic(e.id, e.pos, &args, intrinsic, dest);
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);
        }
    }

    fn emit_call_intrinsic(
        &mut self,
        id: NodeId,
        pos: Position,
        args: &[&'ast Expr],
        intrinsic: Intrinsic,
        dest: ExprStore,
    ) {
        match intrinsic {
            Intrinsic::GenericArrayLen => self.emit_intrinsic_len(pos, args[0], dest.reg()),
            Intrinsic::GenericArrayGet => {
                let element_type = self.ty(args[0].id()).type_params(self.vm)[0];
                self.emit_array_get(pos, element_type.mode(), args[0], args[1], dest)
            }
            Intrinsic::GenericArraySet => {
                let element_type = self.ty(args[0].id()).type_params(self.vm)[0];
                self.emit_array_set(
                    pos,
                    element_type,
                    element_type.mode(),
                    args[0],
                    args[1],
                    args[2],
                )
            }
            Intrinsic::Assert => self.emit_intrinsic_assert(pos, id, args[0], dest.reg()),
            Intrinsic::Debug => self.emit_intrinsic_debug(),
            Intrinsic::Shl => self.emit_intrinsic_shl(args[0], args[1], dest.reg()),
            Intrinsic::SetUint8 => self.emit_set_uint8(args[0], args[1], dest.reg()),
            Intrinsic::StrLen => self.emit_intrinsic_len(pos, args[0], dest.reg()),
            Intrinsic::StrGet => {
                self.emit_array_get(pos, MachineMode::Int8, args[0], args[1], dest)
            }

            Intrinsic::BoolToInt | Intrinsic::ByteToInt => {
                self.emit_intrinsic_byte_to_int(args[0], dest.reg())
            }
            Intrinsic::BoolToLong | Intrinsic::ByteToLong => {
                self.emit_intrinsic_byte_to_long(args[0], dest.reg())
            }
            Intrinsic::LongToByte => self.emit_intrinsic_long_to_byte(args[0], dest.reg()),
            Intrinsic::LongToChar | Intrinsic::LongToInt => {
                self.emit_intrinsic_long_to_int(args[0], dest.reg())
            }
            Intrinsic::LongToFloat => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }
            Intrinsic::LongToDouble => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::LongAsDouble => {
                self.emit_intrinsic_int_as_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::FloatToInt => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::FloatToLong => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::FloatToDouble => self.emit_intrinsic_float_to_double(args[0], dest.freg()),
            Intrinsic::FloatAsInt => {
                self.emit_intrinsic_float_as_int(args[0], dest.reg(), intrinsic)
            }

            Intrinsic::DoubleToInt => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::DoubleToLong => {
                self.emit_intrinsic_float_to_int(args[0], dest.reg(), intrinsic)
            }
            Intrinsic::DoubleToFloat => self.emit_intrinsic_double_to_float(args[0], dest.freg()),
            Intrinsic::DoubleAsLong => {
                self.emit_intrinsic_float_as_int(args[0], dest.reg(), intrinsic)
            }

            Intrinsic::CharToInt | Intrinsic::IntToChar => {
                self.emit_expr(args[0], dest);
            }

            Intrinsic::CharToLong => self.emit_intrinsic_int_to_long(args[0], dest.reg()),
            Intrinsic::CharEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::CharCmp => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::IntToByte => self.emit_intrinsic_int_to_byte(args[0], dest.reg()),
            Intrinsic::IntToLong => self.emit_intrinsic_int_to_long(args[0], dest.reg()),
            Intrinsic::IntToFloat => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }
            Intrinsic::IntToDouble => {
                self.emit_intrinsic_int_to_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::IntAsFloat => {
                self.emit_intrinsic_int_as_float(args[0], dest.freg(), intrinsic)
            }

            Intrinsic::ByteEq => self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic),
            Intrinsic::ByteCmp => self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic),
            Intrinsic::ByteNot => self.emit_intrinsic_bin_call(args[0], args[0], dest, intrinsic),

            Intrinsic::BoolEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::BoolNot => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::IntEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntCmp => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::IntAdd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntSub => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntMul => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntDiv => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntMod => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::IntPlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::IntOr => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntAnd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntXor => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntNot => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::IntShl => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntSar => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::IntShr => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::LongEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongCmp => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::LongAdd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongSub => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongMul => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongDiv => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongMod => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::LongPlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::LongOr => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongAnd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongXor => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongNot => self.emit_intrinsic_unary(args[0], dest, intrinsic),

            Intrinsic::LongShl => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongSar => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::LongShr => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::FloatAdd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::FloatSub => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::FloatMul => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::FloatDiv => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::FloatNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::FloatPlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::FloatIsNan => self.emit_intrinsic_is_nan(args[0], dest.reg(), intrinsic),
            Intrinsic::FloatSqrt => self.emit_intrinsic_sqrt(args[0], dest.freg(), intrinsic),
            Intrinsic::FloatEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::DoubleAdd => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::DoubleSub => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::DoubleMul => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::DoubleDiv => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),
            Intrinsic::DoubleNeg => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::DoublePlus => self.emit_intrinsic_unary(args[0], dest, intrinsic),
            Intrinsic::DoubleIsNan => self.emit_intrinsic_is_nan(args[0], dest.reg(), intrinsic),
            Intrinsic::DoubleSqrt => self.emit_intrinsic_sqrt(args[0], dest.freg(), intrinsic),
            Intrinsic::DoubleEq => self.emit_intrinsic_bin_call(args[0], args[1], dest, intrinsic),

            Intrinsic::DefaultValue => self.emit_intrinsic_default_value(id, dest),

            _ => panic!("unknown intrinsic {:?}", intrinsic),
        }
    }

    fn emit_intrinsic_default_value(&mut self, id: NodeId, dest: ExprStore) {
        let ty = self.ty(id);

        match ty {
            BuiltinType::Bool
            | BuiltinType::Byte
            | BuiltinType::Int
            | BuiltinType::Long
            | BuiltinType::Char => self.asm.load_int_const(ty.mode(), dest.reg(), 0),
            BuiltinType::Float | BuiltinType::Double => {
                self.asm.load_float_const(ty.mode(), dest.freg(), 0.0)
            }
            _ => self.asm.load_nil(dest.reg()),
        }
    }

    fn emit_intrinsic_sqrt(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, dest.into());

        let mode = match intrinsic {
            Intrinsic::FloatSqrt => MachineMode::Float32,
            Intrinsic::DoubleSqrt => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_sqrt(mode, dest, dest);
    }

    fn emit_array_set(
        &mut self,
        pos: Position,
        element_type: BuiltinType,
        mode: MachineMode,
        object: &'ast Expr,
        index: &'ast Expr,
        rhs: &'ast Expr,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let offset_object = self.reserve_temp_for_node(object);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(offset_object),
            REG_RESULT.into(),
        );

        self.emit_expr(index, REG_RESULT.into());
        let offset_index = self.reserve_temp_for_node(index);
        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(offset_index),
            REG_RESULT.into(),
        );

        let res = result_reg(mode);

        self.emit_expr(rhs, res);
        let offset_value = self.reserve_temp_for_node(rhs);
        self.asm.store_mem(mode, Mem::Local(offset_value), res);

        self.asm
            .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(offset_object));
        self.asm.load_mem(
            MachineMode::Int32,
            REG_TMP2.into(),
            Mem::Local(offset_index),
        );

        self.asm.test_if_nil_bailout(pos, REG_TMP1, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm.check_index_out_of_bounds(pos, REG_TMP1, REG_TMP2);
        }

        self.asm.load_mem(mode, res, Mem::Local(offset_value));

        let write_barrier = self.vm.gc.needs_write_barrier() && element_type.reference_type();
        let card_table_offset = self.vm.gc.card_table_offset();

        self.asm.store_array_elem(
            mode,
            REG_TMP1,
            REG_TMP2,
            res,
            write_barrier,
            card_table_offset,
        );

        self.free_temp_for_node(object, offset_object);
        self.free_temp_for_node(index, offset_index);
        self.free_temp_for_node(rhs, offset_value);
    }

    fn emit_array_get(
        &mut self,
        pos: Position,
        mode: MachineMode,
        object: &'ast Expr,
        index: &'ast Expr,
        dest: ExprStore,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let offset = self.reserve_temp_for_node(object);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(index, REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Local(offset));

        self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(pos, REG_RESULT, REG_TMP1);
        }

        let res = result_reg(mode);

        self.asm.load_array_elem(mode, res, REG_RESULT, REG_TMP1);

        self.free_temp_for_node(object, offset);

        if dest != res {
            self.asm.copy(mode, dest, res);
        }
    }

    fn emit_set_uint8(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, _: Reg) {
        self.emit_expr(lhs, REG_RESULT.into());
        let offset = self.reserve_temp_for_node(lhs);
        self.asm
            .store_mem(MachineMode::Int64, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(rhs, REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Int64, REG_RESULT.into(), Mem::Local(offset));

        self.asm
            .store_mem(MachineMode::Int8, Mem::Base(REG_RESULT, 0), REG_TMP1.into());
    }

    fn emit_intrinsic_is_nan(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let mode = match intrinsic {
            Intrinsic::FloatIsNan => MachineMode::Float32,
            Intrinsic::DoubleIsNan => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_cmp_nan(mode, dest, FREG_RESULT);
    }

    fn emit_intrinsic_len(&mut self, pos: Position, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, REG_RESULT.into());
        self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::NIL);
        self.asm.load_mem(
            MachineMode::Ptr,
            dest.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );
    }

    fn emit_intrinsic_assert(&mut self, pos: Position, id: NodeId, e: &'ast Expr, _: Reg) {
        // throw Error if assertion failed
        let lbl_assert = self.asm.create_label();
        self.emit_expr(e, REG_RESULT.into());

        self.asm.emit_comment(Comment::Lit("check assert"));
        self.asm
            .test_and_jump_if(CondCode::NonZero, REG_RESULT, lbl_assert);
        let csite = self.jit_info.map_csites.get(id).unwrap().clone();

        match csite.args.get(1).unwrap() {
            Arg::Stack(offset, _, _) => {
                self.emit_lit_str_value(&"assert failed".to_string(), REG_RESULT);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(*offset), REG_RESULT.into());
            }
            _ => panic!("unexpected argument for assert"),
        };
        self.emit_call_site(&csite, pos, REG_RESULT.into());
        self.asm.throw(REG_RESULT, pos);
        self.asm.bind_label(lbl_assert)
    }

    fn emit_intrinsic_debug(&mut self) {
        self.asm.debug();
    }

    fn emit_intrinsic_shl(&mut self, lhs: &'ast Expr, rhs: &'ast Expr, dest: Reg) {
        self.emit_expr(lhs, REG_RESULT.into());
        let offset = self.reserve_temp_for_node(lhs);
        self.asm
            .store_mem(MachineMode::Int32, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(rhs, REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Int32, REG_RESULT.into(), Mem::Local(offset));

        self.asm
            .int_shl(MachineMode::Int32, dest, REG_RESULT, REG_TMP1);
    }

    fn emit_intrinsic_long_to_int(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
    }

    fn emit_intrinsic_long_to_byte(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_byte(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_long(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, REG_RESULT.into());
        self.asm.extend_int_long(dest, REG_RESULT);
    }

    fn emit_intrinsic_float_to_double(&mut self, e: &'ast Expr, dest: FReg) {
        self.emit_expr(e, FREG_RESULT.into());
        self.asm.float_to_double(dest, FREG_RESULT);
    }

    fn emit_intrinsic_double_to_float(&mut self, e: &'ast Expr, dest: FReg) {
        self.emit_expr(e, FREG_RESULT.into());
        self.asm.double_to_float(dest, FREG_RESULT);
    }

    fn emit_intrinsic_int_to_float(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::IntToFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::IntToDouble => (MachineMode::Int32, MachineMode::Float64),
            Intrinsic::LongToFloat => (MachineMode::Int64, MachineMode::Float32),
            Intrinsic::LongToDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_to_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_float_to_int(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::FloatToInt => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::FloatToLong => (MachineMode::Float32, MachineMode::Int64),
            Intrinsic::DoubleToInt => (MachineMode::Float64, MachineMode::Int32),
            Intrinsic::DoubleToLong => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_to_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_float_as_int(&mut self, e: &'ast Expr, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(e, FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::FloatAsInt => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::DoubleAsLong => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_as_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_int_as_float(&mut self, e: &'ast Expr, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e, REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::IntAsFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::LongAsDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_as_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_byte_to_int(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_byte_to_long(&mut self, e: &'ast Expr, dest: Reg) {
        self.emit_expr(e, dest.into());
        self.asm.extend_byte(MachineMode::Int64, dest, dest);
    }

    fn emit_intrinsic_bin_call(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        dest: ExprStore,
        intr: Intrinsic,
    ) {
        self.emit_intrinsic_bin(lhs, rhs, dest, intr, None);
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        dest: ExprStore,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        let mode = self.ty(lhs.id()).mode();

        let (lhs_reg, rhs_reg) = if mode.is_float() {
            (FREG_RESULT.into(), FREG_TMP1.into())
        } else {
            (REG_RESULT.into(), REG_TMP1.into())
        };

        self.emit_expr(lhs, lhs_reg);
        let offset = self.reserve_temp_for_node(lhs);

        self.asm.store_mem(mode, Mem::Local(offset), lhs_reg);

        self.emit_expr(rhs, rhs_reg);

        self.asm.load_mem(mode, lhs_reg, Mem::Local(offset));

        if mode.is_float() {
            let lhs_reg = lhs_reg.freg();
            let rhs_reg = rhs_reg.freg();

            self.emit_intrinsic_float(dest, lhs_reg, rhs_reg, intr, op);
        } else {
            let lhs_reg = lhs_reg.reg();
            let rhs_reg = rhs_reg.reg();

            self.emit_intrinsic_int(dest.reg(), lhs_reg, rhs_reg, intr, op);
        }

        self.free_temp_for_node(lhs, offset);
    }

    fn emit_intrinsic_int(
        &mut self,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        match intr {
            Intrinsic::ByteEq
            | Intrinsic::BoolEq
            | Intrinsic::CharEq
            | Intrinsic::IntEq
            | Intrinsic::LongEq => {
                let mode = if intr == Intrinsic::LongEq {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.cmp_reg(mode, lhs, rhs);
                self.asm.set(dest, cond_code);
            }

            Intrinsic::EnumEq | Intrinsic::EnumNe => {
                let cond_code = match intr {
                    Intrinsic::EnumEq => CondCode::Equal,
                    Intrinsic::EnumNe => CondCode::NotEqual,
                    _ => unreachable!(),
                };
                self.asm.cmp_reg(MachineMode::Int32, lhs, rhs);
                self.asm.set(dest, cond_code);
            }

            Intrinsic::ByteCmp | Intrinsic::CharCmp | Intrinsic::IntCmp | Intrinsic::LongCmp => {
                let mode = if intr == Intrinsic::LongCmp {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.cmp_reg(mode, lhs, rhs);
                    self.asm.set(dest, cond_code);
                } else {
                    self.asm.int_sub(mode, dest, lhs, rhs);
                }
            }

            Intrinsic::IntAdd => self.asm.int_add(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntSub => self.asm.int_sub(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntMul => self.asm.int_mul(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntDiv => self.asm.int_div(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntMod => self.asm.int_mod(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::IntOr => self.asm.int_or(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntAnd => self.asm.int_and(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntXor => self.asm.int_xor(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::IntShl => self.asm.int_shl(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntSar => self.asm.int_sar(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntShr => self.asm.int_shr(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::LongAdd => self.asm.int_add(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongSub => self.asm.int_sub(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongMul => self.asm.int_mul(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongDiv => self.asm.int_div(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongMod => self.asm.int_mod(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::LongOr => self.asm.int_or(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongAnd => self.asm.int_and(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongXor => self.asm.int_xor(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::LongShl => self.asm.int_shl(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongSar => self.asm.int_sar(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongShr => self.asm.int_shr(MachineMode::Int64, dest, lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_intrinsic_float(
        &mut self,
        dest: ExprStore,
        lhs: FReg,
        rhs: FReg,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        use crate::ty::MachineMode::{Float32, Float64};

        match intr {
            Intrinsic::FloatEq | Intrinsic::DoubleEq => {
                let mode = if intr == Intrinsic::DoubleEq {
                    Float64
                } else {
                    Float32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
            }

            Intrinsic::FloatCmp | Intrinsic::DoubleCmp => {
                let mode = if intr == Intrinsic::DoubleCmp {
                    Float64
                } else {
                    Float32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
                } else {
                    unimplemented!();
                }
            }

            Intrinsic::FloatAdd => self.asm.float_add(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatSub => self.asm.float_sub(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatMul => self.asm.float_mul(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatDiv => self.asm.float_div(Float32, dest.freg(), lhs, rhs),

            Intrinsic::DoubleAdd => self.asm.float_add(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleSub => self.asm.float_sub(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleMul => self.asm.float_mul(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleDiv => self.asm.float_div(Float64, dest.freg(), lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_delegation(&mut self, e: &'ast ExprDelegationType, dest: ExprStore) {
        self.emit_call_site_id(e.id, e.pos, dest);
    }

    fn has_call_site(&self, id: NodeId) -> bool {
        self.jit_info.map_csites.get(id).is_some()
    }

    fn emit_call_site_id(&mut self, id: NodeId, pos: Position, dest: ExprStore) {
        let csite = self.jit_info.map_csites.get(id).unwrap().clone();
        self.emit_call_site(&csite, pos, dest);
    }

    pub fn emit_call_site(&mut self, csite: &CallSite<'ast>, pos: Position, dest: ExprStore) {
        let mut temps: Vec<(BuiltinType, i32, Option<ClassDefId>)> = Vec::new();

        let fid = csite.callee;
        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        for (idx, arg) in csite.args.iter().enumerate() {
            let mode = arg.ty().mode();
            let dest = register_for_mode(mode);

            match *arg {
                Arg::Expr(ast, ty, _) => {
                    self.emit_expr(ast, dest);

                    // check first argument for nil for method calls
                    //
                    // no check necessary for:
                    //   super calls (guaranteed to not be nil) and
                    //   dynamic dispatch (implicit check when loading fctptr from vtable)
                    if idx == 0
                        && fct.has_self()
                        && check_for_nil(ty)
                        && !csite.super_call
                        && !fct.is_virtual()
                    {
                        self.asm.test_if_nil_bailout(pos, dest.reg(), Trap::NIL);
                    }
                }

                Arg::Stack(soffset, ty, _) => {
                    self.asm.load_mem(ty.mode(), dest, Mem::Local(soffset));
                }

                Arg::Selfie(_, _) => {
                    self.emit_self(dest);
                }

                Arg::SelfieNew(ty, _) => {
                    let cls_id = specialize_class_ty(self.vm, ty);
                    // do NOT invoke `reserve_temp_for_arg` here, since
                    // this would also add it to the set of temporaries, which
                    // leads to this address being part of the gc point for
                    // the collection of the object.
                    let offset = self.reserve_temp_for_self(arg);
                    temps.push((ty, offset, Some(cls_id)));
                    continue;
                }
            }

            let offset = self.reserve_temp_for_arg(arg);
            self.asm
                .store_mem(arg.ty().mode(), Mem::Local(offset), dest);
            temps.push((arg.ty(), offset, None));
        }

        let mut arg_offset = -self.jit_info.stacksize();
        let mut idx = 0;
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        for arg in &csite.args {
            let ty = arg.ty();
            let mode = ty.mode();
            let is_float = mode.is_float();
            let offset = temps[idx].1;

            if idx == 0 {
                if let Some(cls_id) = temps[idx].2 {
                    let reg = REG_PARAMS[reg_idx];
                    self.emit_allocation(pos, &temps, cls_id, offset, reg);

                    // after the allocation `offset` is initialized,
                    // add it to the set of temporaries such that it is part
                    // of the gc point
                    self.temps.insert(offset);

                    reg_idx += 1;
                    idx += 1;
                    continue;
                }
            }

            if is_float {
                if freg_idx < FREG_PARAMS.len() {
                    let freg = FREG_PARAMS[freg_idx];
                    self.asm.load_mem(mode, freg.into(), Mem::Local(offset));

                    freg_idx += 1;
                } else {
                    self.asm
                        .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Local(arg_offset), FREG_TMP1.into());

                    arg_offset += 8;
                }
            } else {
                if reg_idx < REG_PARAMS.len() {
                    let reg = REG_PARAMS[reg_idx];
                    self.asm.load_mem(mode, reg.into(), Mem::Local(offset));

                    reg_idx += 1;
                } else {
                    self.asm.load_mem(mode, REG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Local(arg_offset), REG_TMP1.into());

                    arg_offset += 8;
                }
            }

            idx += 1;
        }

        let return_type = self.specialize_type(csite.return_type);
        let cls_type_params = TypeList::with(
            csite
                .cls_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );
        let fct_type_params = TypeList::with(
            csite
                .fct_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );

        debug_assert!(cls_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));
        debug_assert!(fct_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        if csite.super_call {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            self.asm.emit_comment(Comment::CallSuper(fid));
            let gcpoint = create_gcpoint(&self.scopes, &self.temps);
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                return_type,
                dest,
            );
        } else if fct.is_virtual() {
            let vtable_index = fct.vtable_index.unwrap();
            self.asm.emit_comment(Comment::CallVirtual(fid));
            let gcpoint = create_gcpoint(&self.scopes, &self.temps);
            self.asm
                .indirect_call(vtable_index, pos, gcpoint, return_type, dest);
        } else {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            self.asm.emit_comment(Comment::CallDirect(fid));
            let gcpoint = create_gcpoint(&self.scopes, &self.temps);
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                return_type,
                dest,
            );
        }

        if csite.args.len() > 0 {
            if let Arg::SelfieNew(_, _) = csite.args[0] {
                let (ty, offset, _) = temps[0];
                self.asm.load_mem(ty.mode(), dest, Mem::Local(offset));
            }
        }

        for temp in temps.into_iter() {
            self.free_temp_with_type(temp.0, temp.1);
        }
    }

    fn emit_allocation(
        &mut self,
        pos: Position,
        temps: &[(BuiltinType, i32, Option<ClassDefId>)],
        cls_id: ClassDefId,
        offset: i32,
        dest: Reg,
    ) {
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let mut store_length = false;

        // allocate storage for object
        self.asm.emit_comment(Comment::Alloc(cls_id));

        let alloc_size;

        match cls.size {
            ClassSize::Fixed(size) => {
                self.asm
                    .load_int_const(MachineMode::Int32, REG_PARAMS[0], size as i64);
                alloc_size = AllocationSize::Fixed(size as usize);
            }

            ClassSize::Array(esize) if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, esize, true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::ObjArray if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, mem::ptr_width(), true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::Str if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, 1, true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::Array(_) | ClassSize::ObjArray | ClassSize::Str => {
                let size = Header::size() as usize + mem::ptr_width_usize();
                self.asm
                    .load_int_const(MachineMode::Int32, REG_PARAMS[0], size as i64);

                store_length = true;
                alloc_size = AllocationSize::Fixed(size);
            }

            ClassSize::FreeArray => unreachable!(),
        }

        let array_ref = match cls.size {
            ClassSize::ObjArray => true,
            _ => false,
        };

        let gcpoint = create_gcpoint(&self.scopes, &self.temps);
        self.asm.allocate(dest, alloc_size, pos, array_ref, gcpoint);

        // store gc object in temporary storage
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Local(offset), dest.into());

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        let temp = if dest == REG_TMP1 { REG_TMP2 } else { REG_TMP1 };

        self.asm.emit_comment(Comment::StoreVTable(cls_id));
        self.asm.load_constpool(temp, disp + pos);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Base(dest, 0), temp.into());

        // clear mark/fwdptr word in header
        assert!(Header::size() == 2 * mem::ptr_width());
        self.asm.load_int_const(MachineMode::Ptr, temp, 0);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(dest, mem::ptr_width()),
            temp.into(),
        );

        // store length in object
        if store_length {
            if temps.len() > 1 {
                self.asm
                    .load_mem(MachineMode::Int32, temp.into(), Mem::Local(temps[1].1));
            } else {
                self.asm.load_int_const(MachineMode::Ptr, temp, 0);
            }

            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Base(dest, Header::size()),
                temp.into(),
            );
        }

        match cls.size {
            ClassSize::Fixed(size) => {
                self.asm.fill_zero(dest, size as usize);
            }

            _ if temps.len() > 1 => {
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    dest,
                    dest,
                    (Header::size() + mem::ptr_width()) as i64,
                );

                let element_size = match cls.size {
                    ClassSize::Array(esize) => esize,
                    ClassSize::ObjArray => mem::ptr_width(),
                    ClassSize::Str => 1,
                    ClassSize::Fixed(_) => unreachable!(),
                    ClassSize::FreeArray => unreachable!(),
                };

                self.asm
                    .determine_array_size(temp, temp, element_size, false);
                self.asm.int_add(MachineMode::Ptr, temp, temp, dest);
                self.asm.fill_zero_dynamic(dest, temp);
            }

            // arrays with length 0 do not need to clear any data
            _ => {}
        }

        self.asm
            .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
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
                let params = self.vm.lists.lock().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();
                let params = TypeList::with(params);
                let list_id = self.vm.lists.lock().insert(params);

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }
}

impl<'a, 'ast> CodeGen<'ast> for AstCodeGen<'a, 'ast> {
    fn generate(mut self) -> JitBaselineFct {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.emit_prolog();
        self.store_register_params_on_stack();

        {
            let block = self.ast.block();

            for stmt in &block.stmts {
                self.visit_stmt(stmt);
            }

            if let Some(ref value) = block.expr {
                let return_type = self.specialize_type(self.fct.return_type);
                let reg = result_reg(return_type.mode());
                self.emit_expr(value, reg);
                self.emit_epilog();
            }
        }

        assert!(self.temps.is_empty());

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
        }
    }

    fn visit_expr(&mut self, _: &'ast Expr) {
        unreachable!("should not be invoked");
    }
}

fn result_reg(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn check_for_nil(ty: BuiltinType) -> bool {
    match ty {
        BuiltinType::Error => panic!("error shouldn't occur in code generation."),
        BuiltinType::Unit => false,
        BuiltinType::Byte
        | BuiltinType::Char
        | BuiltinType::Int
        | BuiltinType::Long
        | BuiltinType::Float
        | BuiltinType::Double
        | BuiltinType::Bool
        | BuiltinType::Enum(_) => false,
        BuiltinType::Nil | BuiltinType::Ptr => true,
        BuiltinType::Class(_, _) => true,
        BuiltinType::Struct(_, _) => false,
        BuiltinType::Trait(_) => false,
        BuiltinType::This => unreachable!(),
        BuiltinType::ClassTypeParam(_, _) => unreachable!(),
        BuiltinType::FctTypeParam(_, _) => unreachable!(),
        BuiltinType::Lambda(_) => true,
    }
}

fn ensure_jit_or_stub_ptr<'ast>(
    src: &mut FctSrc,
    vm: &VM,
    cls_type_params: TypeList,
    fct_type_params: TypeList,
) -> Address {
    let specials = src.specializations.read();
    let key = (cls_type_params, fct_type_params);

    if let Some(&jit_fct_id) = specials.get(&key) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.fct_ptr();
    }

    vm.compiler_thunk()
}

fn to_cond_code(cmp: CmpOp) -> CondCode {
    match cmp {
        CmpOp::Eq => CondCode::Equal,
        CmpOp::Ne => CondCode::NotEqual,
        CmpOp::Gt => CondCode::Greater,
        CmpOp::Ge => CondCode::GreaterEq,
        CmpOp::Lt => CondCode::Less,
        CmpOp::Le => CondCode::LessEq,
        CmpOp::Is => CondCode::Equal,
        CmpOp::IsNot => CondCode::NotEqual,
    }
}
