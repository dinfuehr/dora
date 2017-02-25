use std::collections::HashMap;
use std::collections::HashSet;
use std::slice;

use capstone::{Engine, Error};

use ast::*;
use ast::Stmt::*;
use ast::visit::*;

use baseline::expr::*;
use baseline::fct::{CatchType, Comment, CommentFormat, GcPoint, JitFct};
use baseline::info;
use baseline::map::CodeData;
use cpu::{FREG_RESULT, Mem, REG_PARAMS, REG_RESULT};
use ctxt::{Context, Fct, FctId, FctSrc, VarId};
use driver::cmd::AsmSyntax;
use masm::*;

use os;
use os::signal::Trap;
use semck::always_returns;
use ty::{BuiltinType, MachineMode};

pub fn generate<'ast>(ctxt: &Context<'ast>, id: FctId) -> *const u8 {
    let fct = ctxt.fcts[id].borrow();
    let src = fct.src();
    let mut src = src.lock().unwrap();

    generate_fct(ctxt, &fct, &mut src)
}

pub fn generate_fct<'ast>(ctxt: &Context<'ast>,
                          fct: &Fct<'ast>,
                          src: &mut FctSrc<'ast>)
                          -> *const u8 {
    if let Some(ref jit) = src.jit_fct {
        return jit.fct_ptr();
    }

    let ast = fct.ast;

    let jit_fct = CodeGen {
            ctxt: ctxt,
            fct: &fct,
            ast: ast,
            masm: MacroAssembler::new(),
            scopes: Scopes::new(),
            src: src,

            lbl_break: None,
            lbl_continue: None,
            lbl_finally: None,
        }
        .generate();

    if should_emit_asm(ctxt, &*fct) {
        dump_asm(ctxt,
                 &*fct,
                 &jit_fct,
                 Some(&src),
                 ctxt.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att));
    }

    let fct_ptr = jit_fct.fct_ptr();
    src.jit_fct = Some(jit_fct);

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

pub fn dump_asm<'ast>(ctxt: &Context<'ast>,
                      fct: &Fct<'ast>,
                      jit_fct: &JitFct,
                      fct_src: Option<&FctSrc<'ast>>,
                      asm_syntax: AsmSyntax) {
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

    let start_addr = jit_fct.fct_ptr() as u64;
    let instrs = engine.disasm(buf, start_addr, jit_fct.fct_len())
        .expect("could not disassemble code");

    let name = fct.full_name(ctxt);

    println!("fn {} {:#x}", &name, start_addr);

    if let Some(fct_src) = fct_src {
        for var in &fct_src.vars {
            let name = ctxt.interner.str(var.name);
            let op = if var.offset < 0 { "-" } else { "" };
            println!("  var `{}`: offset {} ({}0x{:02x}) type {}",
                     name,
                     var.offset,
                     op,
                     var.offset.abs(),
                     var.ty.name(ctxt));
        }

        if fct_src.vars.len() > 0 {
            println!();
        }
    }

    for instr in instrs {
        let addr = (instr.addr - start_addr) as i32;

        if let Some(gc_point) = jit_fct.gcpoint_for_offset(addr) {
            print!("\t\t  ; gc point = (");
            let mut first = true;

            for &offset in &gc_point.offsets {
                if !first {
                    print!(", ");
                }

                print!("{}", offset);
                first = false;
            }

            println!(")");
        }

        if let Some(comments) = jit_fct.get_comment(addr) {
            for comment in comments {
                if comment.is_newline() {
                    println!();
                    continue;
                }

                let cfmt = CommentFormat {
                    comment: comment,
                    ctxt: ctxt,
                    fct_src: fct_src,
                };

                println!("\t\t  ; {}", cfmt);
            }
        }

        println!("  {:#06x}: {}\t\t{}",
                 instr.addr,
                 instr.mnemonic,
                 instr.op_str);
    }

    println!("");
}

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    masm: MacroAssembler,
    scopes: Scopes,
    src: &'a mut FctSrc<'ast>,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,
    lbl_finally: Option<Label>,
}

impl<'a, 'ast> CodeGen<'a, 'ast>
    where 'ast: 'a
{
    pub fn generate(mut self) -> JitFct {
        info::generate(self.ctxt, self.fct, self.src);

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

        let jit_fct = self.masm.jit(self.ctxt, self.src.stacksize());

        let mut code_map = self.ctxt.code_map.lock().unwrap();
        let cdata = CodeData::Fct(self.fct.id);
        code_map.insert(jit_fct.ptr_start(), jit_fct.ptr_end(), cdata);

        if self.ctxt.args.flag_enable_perf {
            os::perf::register_with_perf(&jit_fct, self.ctxt, self.ast.name);
        }

        jit_fct
    }

    fn store_register_params_on_stack(&mut self) {
        let hidden_self = if self.fct.in_class() {
            let var = self.src.var_self();
            self.masm.emit_comment(Comment::StoreParam(var.id));
            self.masm.store_mem(var.ty.mode(), Mem::Local(var.offset), REG_PARAMS[0].into());

            1

        } else {
            0
        };

        for (&reg, p) in REG_PARAMS.iter()
            .skip(hidden_self)
            .zip(&self.ast.params) {
            let var = *self.src.map_vars.get(p.id).unwrap();
            self.masm.emit_comment(Comment::StoreParam(var));
            var_store(&mut self.masm, &self.src, reg.into(), var);
        }

        self.masm.emit_comment(Comment::Newline);
    }

    fn emit_prolog(&mut self) {
        self.masm.prolog(self.src.stacksize());
        self.masm.emit_comment(Comment::Lit("prolog end"));
        self.masm.emit_comment(Comment::Newline);
    }

    fn emit_epilog(&mut self) {
        self.masm.emit_comment(Comment::Newline);
        self.masm.emit_comment(Comment::Lit("epilog"));
        self.masm.epilog(self.src.stacksize());
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);

            if self.lbl_finally.is_some() {
                let mode = self.fct.return_type.mode();
                let offset = self.src.eh_return_value.unwrap();
                self.masm.store_mem(mode, Mem::Local(offset), register_for_mode(mode));
            }
        }

        self.emit_return();
    }

    fn emit_return_with_value(&mut self) {
        if !self.fct.return_type.is_unit() {
            let mode = self.fct.return_type.mode();
            let offset = self.src.eh_return_value.unwrap();
            self.masm.load_mem(mode, register_for_mode(mode), Mem::Local(offset));
        }

        self.emit_return();
    }

    fn emit_return(&mut self) {
        // finally block is currently active, plain return is not allowed, finally block
        // needs to be executed
        if let Some(lbl_finally) = self.lbl_finally {
            self.masm.jump(lbl_finally);

            // if no finally-block currently active just exit from the current function
        } else {
            self.emit_epilog();
        }
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        self.masm.bind_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation

        } else {
            // execute condition, when condition is false jump to
            // end of while
            self.emit_expr(&s.cond);
            self.masm.test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_end);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);
            this.masm.jump(lbl_start);
        });

        self.masm.bind_label(lbl_end);
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.masm.create_label();
        let lbl_end = self.masm.create_label();
        self.masm.bind_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);
            this.masm.jump(lbl_start);
        });

        self.masm.bind_label(lbl_end);
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
        where F: FnOnce(&mut CodeGen<'a, 'ast>)
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
        self.masm.test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            self.masm.jump(lbl_end);
            self.masm.bind_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.masm.bind_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType) {
        self.masm.jump(self.lbl_break.unwrap());
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        self.masm.jump(self.lbl_continue.unwrap());
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

            var_store(&mut self.masm, &self.src, value, var);
        }

        let reference_type = {
            let var = &self.src.vars[var];

            if var.ty.reference_type() {
                self.scopes.add_var(var.id, var.offset);
            }

            var.ty.reference_type()
        };

        // uninitialized variables which reference objects need to be initialized to null
        // otherwise the GC  can't know if the stored value is a valid pointer
        if reference_type && !initialized {
            self.masm.load_nil(REG_RESULT);
            var_store(&mut self.masm, &self.src, REG_RESULT.into(), var);
        }
    }

    fn emit_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        self.emit_expr(&s.expr);
        self.masm.test_if_nil_bailout(s.pos, REG_RESULT, Trap::NIL);

        self.masm.trap(Trap::THROW);
    }

    fn emit_stmt_do(&mut self, s: &'ast StmtDoType) {
        let lbl_after = self.masm.create_label();

        let try_span = self.stmt_with_finally(s, &s.do_block, lbl_after);
        let catch_spans = self.emit_do_catch_blocks(s, try_span, lbl_after);
        let finally_start = self.emit_do_finally_block(s);

        self.masm.bind_label(lbl_after);

        if let Some(finally_start) = finally_start {
            let offset = *self.src.map_offsets.get(s.id).unwrap();
            self.masm.emit_exception_handler(try_span, finally_start, Some(offset), CatchType::Any);

            for &catch_span in &catch_spans {
                self.masm.emit_exception_handler(catch_span,
                                                 finally_start,
                                                 Some(offset),
                                                 CatchType::Any);
            }
        }
    }

    fn emit_do_catch_blocks(&mut self,
                            s: &'ast StmtDoType,
                            try_span: (usize, usize),
                            lbl_after: Label)
                            -> Vec<(usize, usize)> {
        let mut ret = Vec::new();

        for catch in &s.catch_blocks {
            let varid = *self.src.map_vars.get(catch.id).unwrap();
            let offset = self.src.vars[varid].offset;

            self.scopes.push_scope();
            self.scopes.add_var(varid, offset);

            let catch_span = self.stmt_with_finally(s, &catch.block, lbl_after);

            self.scopes.pop_scope();

            let ty = self.src.ty(catch.id);
            let catch_type = CatchType::Class(ty.cls_id(self.ctxt));
            self.masm.emit_exception_handler(try_span, catch_span.0, Some(offset), catch_type);

            ret.push(catch_span);
        }

        ret
    }

    fn stmt_with_finally(&mut self,
                         s: &'ast StmtDoType,
                         stmt: &'ast Stmt,
                         lbl_after: Label)
                         -> (usize, usize) {
        let saved_lbl_finally = self.lbl_finally;
        let lbl_finally = self.masm.create_label();

        // if finally block given then use label as current finally
        // otherwise lbl_finally is just used at label after all catch blocks
        if s.finally_block.is_some() {
            self.lbl_finally = Some(lbl_finally);
        }

        let start = self.masm.pos();
        self.visit_stmt(stmt);
        let end = self.masm.pos();

        self.masm.bind_label(lbl_finally);
        self.lbl_finally = saved_lbl_finally;

        if let Some(ref finally_block) = s.finally_block {
            self.visit_stmt(&finally_block.block);
        }

        if always_returns(stmt) {
            self.emit_return_with_value();
        } else {
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

        let offset = *self.src.map_offsets.get(s.id).unwrap();
        self.scopes.add_var_offset(offset);

        self.visit_stmt(&finally_block.block);

        self.masm.load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Local(offset));
        self.masm.trap(Trap::THROW);

        self.scopes.pop_scope();

        Some(finally_pos)
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> ExprStore {
        let ty = self.src.map_tys.get(e.id()).map(|ty| *ty).unwrap_or(BuiltinType::Int);

        let dest: ExprStore = if ty.is_float() {
            FREG_RESULT.into()
        } else {
            REG_RESULT.into()
        };

        let expr_gen = ExprGen::new(self.ctxt,
                                    self.fct,
                                    self.src,
                                    self.ast,
                                    &mut self.masm,
                                    &mut self.scopes);

        expr_gen.generate(e, dest);

        dest
    }
}

fn register_for_mode(mode: MachineMode) -> ExprStore {
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

pub fn var_store(masm: &mut MacroAssembler, fct: &FctSrc, src: ExprStore, var_id: VarId) {
    let var = &fct.vars[var_id];
    masm.store_mem(var.ty.mode(), Mem::Local(var.offset), src);
}

pub fn var_load(masm: &mut MacroAssembler, fct: &FctSrc, var_id: VarId, dest: ExprStore) {
    let var = &fct.vars[var_id];
    masm.load_mem(var.ty.mode(), dest, Mem::Local(var.offset));
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

pub fn should_emit_debug(ctxt: &Context, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = ctxt.args.flag_emit_debug {
        fct_pattern_match(ctxt, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(ctxt: &Context, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = ctxt.args.flag_emit_asm {
        fct_pattern_match(ctxt, fct, dbg_names)
    } else {
        false
    }
}

fn fct_pattern_match(ctxt: &Context, fct: &Fct, pattern: &str) -> bool {
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
