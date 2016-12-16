use std::collections::HashMap;
use std::collections::HashSet;
use std::slice;

use ast::*;
use ast::Stmt::*;
use ast::visit::*;

use baseline::buffer::*;
use baseline::expr::*;
use baseline::fct::{CatchType, JitFct, GcPoint};
use baseline::info;
use cpu::{emit, Mem, Reg, REG_PARAMS, REG_RESULT, trap};
use ctxt::{Context, Fct, FctId, FctSrc, VarId};
use driver::cmd::AsmSyntax;

use mem::ptr::Ptr;
use os;
use semck::always_returns;
use ty::MachineMode;

pub fn generate<'ast>(ctxt: &Context<'ast>, id: FctId) -> Ptr {
    let fct = ctxt.fct_by_id(id);
    let src = fct.src();
    let mut src = src.lock().unwrap();
    if let Some(ref jit) = src.jit_fct { return jit.fct_ptr(); }

    let ast = fct.ast;

    let jit_fct = CodeGen {
        ctxt: ctxt,
        fct: &fct,
        ast: ast,
        buf: Buffer::new(),
        scopes: Scopes::new(),
        src: &mut src,

        lbl_break: None,
        lbl_continue: None,
        lbl_finally: None,
    }.generate();

    if ctxt.args.flag_emit_asm {
        dump_asm(&jit_fct, &ctxt.interner.str(ast.name),
            ctxt.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att));
    }

    let fct_ptr = jit_fct.fct_ptr();
    src.jit_fct = Some(jit_fct);

    fct_ptr
}

pub fn dump_asm(jit_fct: &JitFct, name: &str, asm_syntax: AsmSyntax) {
    use capstone::*;

    let buf: &[u8] = unsafe {
        slice::from_raw_parts(
            jit_fct.fct_ptr().raw() as *const u8,
            jit_fct.fct_len())
    };

    let asm_syntax = match asm_syntax {
        AsmSyntax::Intel => 1,
        AsmSyntax::Att => 2,
    };

    let engine = Engine::new(Arch::X86, MODE_64)
                 .expect("cannot create capstone engine");
    if let Err(_) = engine.set_option(Opt::Syntax, asm_syntax) {
        panic!("capstone: syntax couldn't be set");
    }

    let start_addr = jit_fct.fct_ptr().raw() as u64;
    let instrs = engine.disasm(buf, start_addr,
        jit_fct.fct_len()).expect("could not disassemble code");

    println!("fn {} (id {}) {:#x}", name, jit_fct.fct_id().0, start_addr);

    for instr in instrs {
        if let Some(comment) = jit_fct.get_comment((instr.addr - start_addr) as i32) {
            println!("\t\t; {}", comment);
        }

        println!("  {:#06x}: {}\t\t{}",
                 instr.addr, instr.mnemonic, instr.op_str);
    }
}

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    buf: Buffer,
    scopes: Scopes,
    src: &'a mut FctSrc<'ast>,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,
    lbl_finally: Option<Label>,
}

impl<'a, 'ast> CodeGen<'a, 'ast> where 'ast: 'a {
    pub fn generate(mut self) -> JitFct {
        info::generate(self.ctxt, self.fct, self.src);

        if self.ctxt.args.flag_emit_debug {
            emit::debug(&mut self.buf);
        }

        self.emit_prolog();
        self.store_register_params_on_stack();
        self.visit_fct(self.ast);

        let always_returns = self.src.always_returns;

        if !always_returns {
            self.emit_epilog();
        }

        let jit_fct = self.buf.jit(self.fct.id, self.src.stacksize());

        let mut code_map = self.ctxt.code_map.lock().unwrap();
        code_map.insert(jit_fct.ptr_start().raw(), jit_fct.ptr_end().raw(), jit_fct.fct_id());

        if self.ctxt.args.flag_enable_perf {
            os::perf::register_with_perf(&jit_fct, self.ctxt, self.ast.name);
        }

        jit_fct
    }

    fn store_register_params_on_stack(&mut self) {
        let hidden_self = if self.fct.in_class() {
            let var = self.src.var_self();
            emit::store_mem(&mut self.buf, var.ty.mode(), Mem::Local(var.offset),
                            REG_PARAMS[0]);

            1

        } else {
            0
        };

        for (&reg, p) in REG_PARAMS.iter().skip(hidden_self)
                        .zip(&self.ast.params) {
            var_store(&mut self.buf, &self.src, reg, p.var());
        }
    }

    fn emit_prolog(&mut self) {
        emit::prolog(&mut self.buf, self.src.stacksize());
    }

    fn emit_epilog(&mut self) {
        emit::epilog(&mut self.buf, self.src.stacksize());
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);

            if self.lbl_finally.is_some() {
                let mode = self.fct.return_type.mode();
                let offset = self.src.eh_return_value.unwrap();
                emit::store_mem(&mut self.buf, mode,
                                Mem::Local(offset), REG_RESULT);
            }
        }

        self.emit_return();
    }

    fn emit_return_with_value(&mut self) {
        if !self.fct.return_type.is_unit() {
            let mode = self.fct.return_type.mode();
            let offset = self.src.eh_return_value.unwrap();
            emit::load_mem(&mut self.buf, mode, REG_RESULT, Mem::Local(offset));
        }

        self.emit_return();
    }

    fn emit_return(&mut self) {
        // finally block is currently active, plain return is not allowed, finally block
        // needs to be executed
        if let Some(lbl_finally) = self.lbl_finally {
            emit::jump(&mut self.buf, lbl_finally);

        // if no finally-block currently active just exit from the current function
        } else {
            self.emit_epilog();
        }
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.buf.bind_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation

        } else {
            // execute condition, when condition is false jump to
            // end of while
            let reg = self.emit_expr(&s.cond);
            emit::test_and_jump_if(&mut self.buf, CondCode::Zero, reg, lbl_end);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);
            emit::jump(&mut this.buf, lbl_start);
        });

        self.buf.bind_label(lbl_end);
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();
        self.buf.bind_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);
            emit::jump(&mut this.buf, lbl_start);
        });

        self.buf.bind_label(lbl_end);
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
            where F: FnOnce(&mut CodeGen<'a, 'ast>) {
        let old_lbl_break = self.lbl_break;
        let old_lbl_continue = self.lbl_continue;

        self.lbl_break = Some(lbl_break);
        self.lbl_continue = Some(lbl_continue);

        f(self);

        self.lbl_break = old_lbl_break;
        self.lbl_continue = old_lbl_continue;
    }

    fn emit_stmt_if(&mut self, s: &'ast StmtIfType) {
        let lbl_end = self.buf.create_label();
        let lbl_else = if let Some(_) = s.else_block {
            self.buf.create_label()
        } else {
            lbl_end
        };

        let reg = self.emit_expr(&s.cond);
        emit::test_and_jump_if(&mut self.buf, CondCode::Zero, reg, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            emit::jump(&mut self.buf, lbl_end);
            self.buf.bind_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.buf.bind_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType)  {
        emit::jump(&mut self.buf, self.lbl_break.unwrap());
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        emit::jump(&mut self.buf, self.lbl_continue.unwrap());
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

        if let Some(ref expr) = s.expr {
            let reg = self.emit_expr(expr);
            initialized = true;

            var_store(&mut self.buf, &self.src, reg, s.var());
        }

        let reference_type = {
            let var = &self.src.vars[s.var()];

            if var.ty.reference_type() {
                self.scopes.add_var(var.id, var.offset);
            }

            var.ty.reference_type()
        };

        // uninitialized variables which reference objects need to be initialized to null
        // otherwise the GC  can't know if the stored value is a valid pointer
        if reference_type && !initialized {
            emit::load_nil(&mut self.buf, REG_RESULT);
            var_store(&mut self.buf, &self.src, REG_RESULT, s.var());
        }
    }

    fn emit_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        let reg = self.emit_expr(&s.expr);
        emit::nil_ptr_check_bailout(&mut self.buf, s.pos, reg);

        if reg != REG_RESULT {
            emit::mov_reg_reg(&mut self.buf, MachineMode::Ptr, reg, REG_RESULT);
        }

        trap::emit(&mut self.buf, trap::THROW);
    }

    fn emit_stmt_do(&mut self, s: &'ast StmtDoType) {
        let lbl_after = self.buf.create_label();

        let try_span = self.stmt_with_finally(s, &s.do_block, lbl_after);
        let catch_spans = self.emit_do_catch_blocks(s, try_span, lbl_after);
        let finally_start = self.emit_do_finally_block(s);

        self.buf.bind_label(lbl_after);

        if let Some(finally_start) = finally_start {
            let offset = s.finally_block.as_ref().unwrap().offset();
            self.buf.emit_exception_handler(try_span, finally_start,
                                            Some(offset), CatchType::Any);

            for &catch_span in &catch_spans {
                self.buf.emit_exception_handler(catch_span, finally_start,
                                                Some(offset), CatchType::Any);
            }
        }
    }

    fn emit_do_catch_blocks(&mut self, s: &'ast StmtDoType, try_span: (usize, usize),
                             lbl_after: Label) -> Vec<(usize, usize)> {
        let mut ret = Vec::new();

        for catch in &s.catch_blocks {
            let varid = catch.var();
            let offset = self.src.vars[varid].offset;

            self.scopes.push_scope();
            self.scopes.add_var(varid, offset);

            let catch_span = self.stmt_with_finally(s, &catch.block, lbl_after);

            self.scopes.pop_scope();

            let catch_type = CatchType::Class(catch.ty().cls_id(self.ctxt));
            self.buf.emit_exception_handler(try_span, catch_span.0, Some(offset), catch_type);

            ret.push(catch_span);
        }

        ret
    }

    fn stmt_with_finally(&mut self, s: &'ast StmtDoType, stmt: &'ast Stmt,
                            lbl_after: Label) -> (usize, usize) {
        let saved_lbl_finally = self.lbl_finally;
        let lbl_finally = self.buf.create_label();

        // if finally block given then use label as current finally
        // otherwise lbl_finally is just used at label after all catch blocks
        if s.finally_block.is_some() {
            self.lbl_finally = Some(lbl_finally);
        }

        let start = self.buf.pos();
        self.visit_stmt(stmt);
        let end = self.buf.pos();

        self.buf.bind_label(lbl_finally);
        self.lbl_finally = saved_lbl_finally;

        if let Some(ref finally_block) = s.finally_block {
            self.visit_stmt(&finally_block.block);
        }

        if always_returns(stmt) {
            self.emit_return_with_value();
        } else {
            emit::jump(&mut self.buf, lbl_after);
        }

        (start, end)
    }

    fn emit_do_finally_block(&mut self, s: &'ast StmtDoType)
                              -> Option<usize> {
        if s.finally_block.is_none() { return None; }
        let finally_block = s.finally_block.as_ref().unwrap();

        let finally_pos = self.buf.pos();

        self.scopes.push_scope();
        self.scopes.add_var_offset(finally_block.offset());

        self.visit_stmt(&finally_block.block);

        emit::load_mem(&mut self.buf, MachineMode::Ptr, REG_RESULT,
                       Mem::Local(finally_block.offset()));
        trap::emit(&mut self.buf, trap::THROW);

        self.scopes.pop_scope();

        Some(finally_pos)
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> Reg {
        let expr_gen = ExprGen::new(self.ctxt, self.fct, self.src, self.ast,
                                    &mut self.buf, &mut self.scopes);

        expr_gen.generate(e)
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for CodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.emit_stmt_expr(stmt),
            StmtIf(ref stmt) => self.emit_stmt_if(stmt),
            StmtLoop(ref stmt) => self.emit_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.emit_stmt_while(stmt),
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            StmtBreak(ref stmt) => self.emit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.emit_stmt_continue(stmt),
            StmtBlock(ref stmt) => self.emit_stmt_block(stmt),
            StmtVar(ref stmt) => self.emit_stmt_var(stmt),
            StmtThrow(ref stmt) => self.emit_stmt_throw(stmt),
            StmtDo(ref stmt) => self.emit_stmt_do(stmt),
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

pub fn var_store(buf: &mut Buffer, fct: &FctSrc, src: Reg, var_id: VarId) {
    let var = &fct.vars[var_id];
    emit::store_mem(buf, var.ty.mode(), Mem::Local(var.offset), src);
}

pub fn var_load(buf: &mut Buffer, fct: &FctSrc, var_id: VarId, dest: Reg) {
    let var = &fct.vars[var_id];
    emit::load_mem(buf, var.ty.mode(), dest, Mem::Local(var.offset));
}

pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn new() -> Scopes {
        Scopes {
            scopes: vec![Scope::new()]
        }
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
        TempOffsets {
            offsets: HashSet::new()
        }
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
    Flow(Label), Return
}
