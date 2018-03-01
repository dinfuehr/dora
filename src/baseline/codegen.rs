use libc;

use std::collections::HashMap;
use std::collections::HashSet;
use std::io::{self, BufWriter, Write};
use std::fs::OpenOptions;
use std::slice;

use capstone::{Engine, Error};

use dora_parser::ast::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::visit::*;
use dora_parser::lexer::position::Position;

use baseline::expr::*;
use baseline::fct::{CatchType, Comment, CommentFormat, GcPoint, JitBaselineFct, JitFct};
use baseline::info::{self, JitInfo};
use baseline::map::CodeData;
use class::{ClassDef, TypeParams};
use cpu::{Mem, FREG_PARAMS, FREG_RESULT, REG_PARAMS, REG_RESULT};
use ctxt::{CallSite, Fct, FctId, FctParent, FctSrc, SemContext, VarId};
use driver::cmd::AsmSyntax;
use masm::*;
use opt;

use os;
use os::signal::Trap;
use semck::always_returns;
use semck::specialize::specialize_class_ty;
use ty::{BuiltinType, MachineMode};

pub fn generate<'ast>(
    ctxt: &SemContext<'ast>,
    id: FctId,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> *const u8 {
    let fct = ctxt.fcts[id].borrow();
    let src = fct.src();
    let mut src = src.borrow_mut();

    generate_fct(ctxt, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    ctxt: &SemContext<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> *const u8 {
    debug_assert!(cls_type_params.iter().all(
        |ty| !ty.contains_type_param(ctxt),
    ));
    debug_assert!(fct_type_params.iter().all(
        |ty| !ty.contains_type_param(ctxt),
    ));

    {
        let specials = src.specializations.read().unwrap();
        let key = (cls_type_params.clone(), fct_type_params.clone());

        if let Some(&jit_fct_id) = specials.get(&key) {
            return ctxt.jit_fcts[jit_fct_id].borrow().fct_ptr();
        }
    }

    if should_optimize(ctxt, &*fct) {
        if let Ok(ptr) = opt::generate_fct(ctxt, fct, src, cls_type_params, fct_type_params) {
            return ptr;
        }

        panic!(
            "optimizing compiler cannot compile function {:?}",
            fct.full_name(ctxt)
        );
    }

    let ast = fct.ast;

    let mut jit_info = JitInfo::new();
    info::generate(
        ctxt,
        fct,
        src,
        &mut jit_info,
        cls_type_params,
        fct_type_params,
    );
    let jit_fct = CodeGen {
        ctxt: ctxt,
        fct: &fct,
        ast: ast,
        masm: MacroAssembler::new(),
        scopes: Scopes::new(),
        src: src,
        jit_info: jit_info,

        lbl_break: None,
        lbl_continue: None,

        active_finallys: Vec::new(),
        active_upper: None,
        active_loop: None,
        lbl_return: None,

        cls_type_params: cls_type_params,
        fct_type_params: fct_type_params,
    }.generate();

    if should_emit_asm(ctxt, &*fct) {
        dump_asm(
            ctxt,
            &*fct,
            &jit_fct,
            Some(&src),
            ctxt.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
        );
    }

    let fct_ptr = jit_fct.fct_ptr();
    let ptr_start;
    let ptr_end;

    let jit_fct_id = {
        let mut specials = src.specializations.write().unwrap();
        let key = (cls_type_params.clone(), fct_type_params.clone());

        ptr_start = jit_fct.ptr_start();
        ptr_end = jit_fct.ptr_end();

        let jit_fct_id = ctxt.jit_fcts.len().into();
        ctxt.jit_fcts.push(JitFct::Base(jit_fct));
        specials.insert(key, jit_fct_id);

        jit_fct_id
    };

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        let cdata = CodeData::Fct(jit_fct_id);
        code_map.insert(ptr_start, ptr_end, cdata);
    }

    fct_ptr
}

#[cfg(target_arch = "x86_64")]
fn get_engine() -> Result<Engine, Error> {
    use capstone::{Arch, MODE_64};

    Engine::new(Arch::X86, MODE_64)
}

#[cfg(target_arch = "aarch64")]
fn get_engine() -> Result<Engine, Error> {
    use capstone::{Arch, MODE_ARM};

    Engine::new(Arch::Arm64, MODE_ARM)
}

pub fn dump_asm<'ast>(
    ctxt: &SemContext<'ast>,
    fct: &Fct<'ast>,
    jit_fct: &JitBaselineFct,
    fct_src: Option<&FctSrc>,
    asm_syntax: AsmSyntax,
) {
    use capstone::*;

    let buf: &[u8] = unsafe { slice::from_raw_parts(jit_fct.fct_ptr(), jit_fct.fct_len()) };

    let asm_syntax = match asm_syntax {
        AsmSyntax::Intel => 1,
        AsmSyntax::Att => 2,
    };

    let engine = get_engine().expect("cannot create capstone engine");
    if let Err(_) = engine.set_option(Opt::Syntax, asm_syntax) {
        panic!("capstone: syntax couldn't be set");
    }

    let mut w: Box<Write> = if ctxt.args.flag_emit_asm_file {
        let pid = unsafe { libc::getpid() };
        let name = format!("code-{}.asm", pid);
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&name)
            .expect("couldn't append to asm file");

        Box::new(BufWriter::new(file))
    } else {
        Box::new(io::stdout())
    };

    let start_addr = jit_fct.fct_ptr() as u64;
    let end_addr = jit_fct.fct_end() as u64;

    let instrs = engine.disasm(buf, start_addr, jit_fct.fct_len()).expect(
        "could not disassemble code",
    );

    let name = fct.full_name(ctxt);

    writeln!(&mut w, "fun {} {:#x} {:#x}", &name, start_addr, end_addr).unwrap();

    if let Some(fct_src) = fct_src {
        for var in &fct_src.vars {
            let name = ctxt.interner.str(var.name);
            writeln!(&mut w, "  var `{}`: type {}", name, var.ty.name(ctxt)).unwrap();
        }

        if fct_src.vars.len() > 0 {
            writeln!(&mut w).unwrap();
        }
    }

    for instr in instrs {
        let addr = (instr.addr - start_addr) as i32;

        if let Some(gc_point) = jit_fct.gcpoint_for_offset(addr) {
            write!(&mut w, "\t\t  ; gc point = (").unwrap();
            let mut first = true;

            for &offset in &gc_point.offsets {
                if !first {
                    write!(&mut w, ", ").unwrap();
                }

                if offset < 0 {
                    write!(&mut w, "-").unwrap();
                }

                write!(&mut w, "{:x}", offset.abs()).unwrap();
                first = false;
            }

            writeln!(&mut w, ")").unwrap();
        }

        if let Some(comments) = jit_fct.get_comment(addr) {
            for comment in comments {
                if comment.is_newline() {
                    writeln!(&mut w).unwrap();
                    continue;
                }

                let cfmt = CommentFormat {
                    comment: comment,
                    ctxt: ctxt,
                    fct_src: fct_src,
                };

                writeln!(&mut w, "\t\t  ; {}", cfmt).unwrap();
            }
        }

        writeln!(
            &mut w,
            "  {:#06x}: {}\t\t{}",
            instr.addr,
            instr.mnemonic,
            instr.op_str
        ).unwrap();
    }

    writeln!(&mut w).unwrap();
}

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    masm: MacroAssembler,
    scopes: Scopes,
    src: &'a mut FctSrc,
    jit_info: JitInfo<'ast>,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,

    // stores all active finally blocks
    active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    active_upper: Option<usize>,

    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,
}

impl<'a, 'ast> CodeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn generate(mut self) -> JitBaselineFct {
        if should_emit_debug(self.ctxt, self.fct) {
            self.masm.debug();
        }

        self.emit_prolog();
        self.store_register_params_on_stack();
        self.visit_fct(self.ast);

        let always_returns = self.src.always_returns;

        if !always_returns {
            self.emit_epilog();
        }

        let jit_fct = self.masm.jit(
            self.ctxt,
            self.jit_info.stacksize(),
            self.fct.id,
            self.ast.throws,
        );

        if self.ctxt.args.flag_enable_perf {
            os::perf::register_with_perf(&jit_fct, self.ctxt, self.ast.name);
        }

        jit_fct
    }

    fn store_register_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        if self.fct.has_self() {
            let var = self.src.var_self();
            let mode = var.ty.mode();

            self.masm.emit_comment(Comment::StoreParam(var.id));

            let dest = if mode.is_float() {
                FREG_PARAMS[0].into()
            } else {
                REG_PARAMS[0].into()
            };

            let offset = self.jit_info.offset(var.id);
            self.masm.store_mem(mode, Mem::Local(offset), dest);

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

                self.masm.emit_comment(Comment::StoreParam(varid));
                var_store(&mut self.masm, &self.jit_info, reg.into(), varid);

                freg_idx += 1;
            } else if !is_float && reg_idx < REG_PARAMS.len() {
                let reg = REG_PARAMS[reg_idx];

                self.masm.emit_comment(Comment::StoreParam(varid));
                var_store(&mut self.masm, &self.jit_info, reg.into(), varid);

                reg_idx += 1;
            } else {
                // ignore params not stored in register
            }
        }
    }

    fn emit_prolog(&mut self) {
        self.masm.prolog(self.jit_info.stacksize());
        self.masm.emit_comment(Comment::Lit("prolog end"));
        self.masm.emit_comment(Comment::Newline);
    }

    fn emit_epilog(&mut self) {
        self.masm.emit_comment(Comment::Newline);
        self.masm.emit_comment(Comment::Lit("epilog"));
        self.masm.epilog(
            self.jit_info.stacksize(),
            self.ctxt.polling_page.addr(),
        );
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        let len = self.active_upper.unwrap_or(self.active_finallys.len());
        let return_type = self.specialize_type(self.fct.return_type);

        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);

            if len > 0 {
                let offset = self.jit_info.eh_return_value.unwrap();
                let rmode = return_type.mode();
                self.masm.store_mem(
                    rmode,
                    Mem::Local(offset),
                    register_for_mode(rmode),
                );
            }
        }

        if let Some(lbl_return) = self.lbl_return {
            self.masm.jump(lbl_return);
            return;
        }

        if len > 0 {
            let mut ind = 0;
            while ind < len {
                let lbl = self.masm.create_label();
                self.lbl_return = Some(lbl);

                let finally = self.active_finallys[len - 1 - ind];
                self.visit_stmt(finally);

                self.masm.bind_label(lbl);

                ind += 1;
            }

            if s.expr.is_some() {
                let offset = self.jit_info.eh_return_value.unwrap();
                let rmode = return_type.mode();
                self.masm.load_mem(
                    rmode,
                    register_for_mode(rmode),
                    Mem::Local(offset),
                );
            }

            self.lbl_return = None;
        }

        self.emit_epilog();
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.masm.bind_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation

        } else {
            // execute condition, when condition is false jump to
            // end of while
            self.emit_expr(&s.cond);
            self.masm.test_and_jump_if(
                CondCode::Zero,
                REG_RESULT,
                lbl_end,
            );
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.masm.jump(lbl_start);
        });

        self.masm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_stmt_for(&mut self, s: &'ast StmtForType) {
        let for_info = self.jit_info.map_fors.get(s.id).unwrap().clone();

        // emit: <iterator> = obj.makeIterator()
        let dest = self.emit_call_site(&for_info.make_iterator, s.pos);

        // offset of iterator storage
        let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Local(offset),
            dest,
        );

        let lbl_start = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.masm.bind_label(lbl_start);

        // emit: iterator.hasNext() & jump to lbl_end if false
        let dest = self.emit_call_site(&for_info.has_next, s.pos);
        self.masm.test_and_jump_if(
            CondCode::Zero,
            dest.reg(),
            lbl_end,
        );

        // emit: <for_var> = iterator.next()
        let dest = self.emit_call_site(&for_info.next, s.pos);

        let for_var_id = *self.src.map_vars.get(s.id).unwrap();
        var_store(&mut self.masm, &self.jit_info, dest, for_var_id);

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.masm.jump(lbl_start);
        });

        self.masm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        let saved_active_loop = self.active_loop;

        self.active_loop = Some(self.active_finallys.len());
        self.masm.bind_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);

            this.emit_safepoint();
            this.masm.jump(lbl_start);
        });

        self.masm.bind_label(lbl_end);
        self.active_loop = saved_active_loop;
    }

    fn emit_safepoint(&mut self) {
        self.masm.emit_comment(Comment::ReadPollingPage);
        self.masm.check_polling_page(self.ctxt.polling_page.addr());

        let temps = TempOffsets::new();
        let gcpoint = create_gcpoint(&self.scopes, &temps);
        self.masm.emit_gcpoint(gcpoint);
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
    where
        F: FnOnce(&mut CodeGen<'a, 'ast>),
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
        let lbl_end = self.masm.create_label();
        let lbl_else = if let Some(_) = s.else_block {
            self.masm.create_label()
        } else {
            lbl_end
        };

        self.emit_expr(&s.cond);
        self.masm.test_and_jump_if(
            CondCode::Zero,
            REG_RESULT,
            lbl_else,
        );

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            self.masm.jump(lbl_end);
            self.masm.bind_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.masm.bind_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType) {
        // emit finallys between loop and break
        self.emit_finallys_within_loop();

        // now jump out of loop
        self.masm.jump(self.lbl_break.unwrap());
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        // emit finallys between loop and continue
        self.emit_finallys_within_loop();

        // now jump to start of loop
        self.masm.jump(self.lbl_continue.unwrap());
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

            var_store(&mut self.masm, &self.jit_info, value, var);
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
            self.masm.load_nil(REG_RESULT);
            var_store(&mut self.masm, &self.jit_info, REG_RESULT.into(), var);
        }
    }

    fn emit_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        self.emit_expr(&s.expr);
        self.masm.test_if_nil_bailout(s.pos, REG_RESULT, Trap::NIL);

        self.masm.trap(Trap::THROW);
    }

    fn emit_stmt_do(&mut self, s: &'ast StmtDoType) {
        let lbl_after = self.masm.create_label();

        let do_span = self.stmt_with_finally(s, &s.do_block, lbl_after);
        let catch_spans = self.emit_do_catch_blocks(s, do_span, lbl_after);
        let finally_start = self.emit_do_finally_block(s);

        self.masm.bind_label(lbl_after);

        if let Some(finally_start) = finally_start {
            let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
            self.masm.emit_exception_handler(
                do_span,
                finally_start,
                Some(offset),
                CatchType::Any,
            );

            for &catch_span in &catch_spans {
                self.masm.emit_exception_handler(
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
            let cls_def_id = specialize_class_ty(self.ctxt, ty);
            let cls_def = self.ctxt.class_defs[cls_def_id].borrow();

            let catch_type = CatchType::Class(&*cls_def as *const ClassDef);
            self.masm.emit_exception_handler(
                try_span,
                catch_span.0,
                Some(offset),
                catch_type,
            );

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

        let start = self.masm.pos();
        self.visit_stmt(stmt);
        let end = self.masm.pos();

        if s.finally_block.is_some() {
            self.active_finallys.pop();
        }

        if !always_returns(stmt) {
            if let Some(ref finally_block) = s.finally_block {
                self.visit_stmt(&finally_block.block);
            }

            self.masm.jump(lbl_after);
        }

        (start, end)
    }

    fn emit_do_finally_block(&mut self, s: &'ast StmtDoType) -> Option<usize> {
        if s.finally_block.is_none() {
            return None;
        }
        let finally_block = s.finally_block.as_ref().unwrap();

        let finally_pos = self.masm.pos();

        self.scopes.push_scope();

        let offset = *self.jit_info.map_offsets.get(s.id).unwrap();
        self.scopes.add_var_offset(offset);

        self.visit_stmt(&finally_block.block);

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Local(offset),
        );
        self.masm.trap(Trap::THROW);

        self.scopes.pop_scope();

        Some(finally_pos)
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> ExprStore {
        let ty = self.src.map_tys.get(e.id()).map(|ty| *ty).unwrap_or(
            BuiltinType::Int,
        );

        let ty = self.specialize_type(ty);

        let dest: ExprStore = if ty.is_float() {
            FREG_RESULT.into()
        } else {
            REG_RESULT.into()
        };

        let expr_gen = ExprGen::new(
            self.ctxt,
            self.fct,
            self.src,
            self.ast,
            &mut self.masm,
            &mut self.scopes,
            &self.jit_info,
            self.cls_type_params,
            self.fct_type_params,
        );

        expr_gen.generate(e, dest);

        dest
    }

    fn emit_call_site(&mut self, call_site: &CallSite<'ast>, pos: Position) -> ExprStore {
        let callee = self.ctxt.fcts[call_site.callee].borrow();
        let return_type = self.specialize_type(callee.return_type);

        let dest = register_for_mode(return_type.mode());

        let mut expr_gen = ExprGen::new(
            self.ctxt,
            self.fct,
            self.src,
            self.ast,
            &mut self.masm,
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
                let params = self.ctxt.lists.borrow().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.ctxt.lists.borrow_mut().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }
}

pub fn register_for_mode(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for CodeGen<'a, 'ast> {
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CondCode {
    Zero,
    NonZero,
    Equal,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    UnsignedGreater,
    UnsignedGreaterEq,
    UnsignedLess,
    UnsignedLessEq,
}

pub fn var_store(masm: &mut MacroAssembler, jit_info: &JitInfo, src: ExprStore, var_id: VarId) {
    let offset = jit_info.offset(var_id);
    let ty = jit_info.ty(var_id);
    masm.store_mem(ty.mode(), Mem::Local(offset), src);
}

pub fn var_load(masm: &mut MacroAssembler, jit_info: &JitInfo, var_id: VarId, dest: ExprStore) {
    let offset = jit_info.offset(var_id);
    let ty = jit_info.ty(var_id);
    masm.load_mem(ty.mode(), dest, Mem::Local(offset));
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn new() -> Scopes {
        Scopes { scopes: vec![Scope::new()] }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.pop().is_some());
        assert!(self.scopes.len() >= 1);
    }

    pub fn add_var(&mut self, id: VarId, offset: i32) {
        let scope = self.scopes.last_mut().unwrap();
        assert!(scope.vars.insert(id, offset).is_none());
    }

    pub fn add_var_offset(&mut self, offset: i32) {
        let scope = self.scopes.last_mut().unwrap();
        scope.offsets.push(offset);
    }
}

struct Scope {
    vars: HashMap<VarId, i32>,
    offsets: Vec<i32>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: HashMap::new(),
            offsets: Vec::new(),
        }
    }
}

pub struct TempOffsets {
    offsets: HashSet<i32>,
}

impl TempOffsets {
    pub fn new() -> TempOffsets {
        TempOffsets { offsets: HashSet::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.offsets.is_empty()
    }

    pub fn insert(&mut self, offset: i32) {
        assert!(self.offsets.insert(offset));
    }

    pub fn remove(&mut self, offset: i32) {
        assert!(self.offsets.remove(&offset));
    }
}

pub fn create_gcpoint(vars: &Scopes, temps: &TempOffsets) -> GcPoint {
    let mut offsets = Vec::new();

    for scope in &vars.scopes {
        for (_, &offset) in &scope.vars {
            offsets.push(offset);
        }

        offsets.extend_from_slice(&scope.offsets);
    }

    for &offset in &temps.offsets {
        offsets.push(offset);
    }

    GcPoint::from_offsets(offsets)
}

#[derive(Copy, Clone, Debug)]
pub enum Next {
    Flow(Label),
    Return,
}

pub fn should_emit_debug(ctxt: &SemContext, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = ctxt.args.flag_emit_debug {
        fct_pattern_match(ctxt, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(ctxt: &SemContext, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = ctxt.args.flag_emit_asm {
        fct_pattern_match(ctxt, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_optimize(ctxt: &SemContext, fct: &Fct) -> bool {
    if let Some(ref opt) = ctxt.args.flag_opt {
        fct_pattern_match(ctxt, fct, opt)
    } else {
        false
    }
}

fn fct_pattern_match(ctxt: &SemContext, fct: &Fct, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let name = ctxt.interner.str(fct.name);

    for part in pattern.split(',') {
        if *name == part {
            return true;
        }
    }

    false
}
