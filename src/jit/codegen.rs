use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::slice;

use libc;

use ast::*;
use ast::Stmt::*;
use ast::visit::*;

use cpu::{Reg, REG_PARAMS, REG_RESULT, REG_TMP1};
use cpu::emit;
use ctxt::{Context, Fct, FctId, TryStatus, VarId};
use driver::cmd::AsmSyntax;

use jit::buffer::*;
use jit::expr::*;
use jit::fct::{JitFct, GcPoint};
use jit::info;
use mem::ptr::Ptr;
use object::Obj;
use stdlib;
use ty::MachineMode;

pub fn generate<'ast>(ctxt: &Context<'ast>, id: FctId) -> Ptr {
    ctxt.fct_by_id_mut(id, |fct| {
        if let Some(ref jit) = fct.src().jit_fct { return jit.fct_ptr(); }

        let ast = fct.src().ast;

        let jit_fct = CodeGen {
            ctxt: ctxt,
            fct: fct,
            ast: ast,
            buf: Buffer::new(),
            scopes: Scopes::new(),

            lbl_break: None,
            lbl_continue: None,
            lbl_finally: None,
        }.generate();

        if ctxt.args.flag_emit_asm {
            dump_asm(&jit_fct, &ctxt.interner.str(ast.name),
                ctxt.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att));
        }

        let fct_ptr = jit_fct.fct_ptr();
        fct.src_mut().jit_fct = Some(jit_fct);

        fct_ptr
    })
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
    engine.set_option(Opt::Syntax, asm_syntax);

    let instrs = engine.disasm(buf, jit_fct.fct_ptr().raw() as u64,
        jit_fct.fct_len()).expect("could not disassemble code");

    println!("fn {} (id {})", name, jit_fct.fct_id().0);

    for instr in instrs {
        println!("  {:#06x}: {}\t\t{}",
                 instr.addr, instr.mnemonic, instr.op_str);
    }
}

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    buf: Buffer,
    scopes: Scopes,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,
    lbl_finally: Option<Label>,
}

impl<'a, 'ast> CodeGen<'a, 'ast> where 'ast: 'a {
    pub fn generate(mut self) -> JitFct {
        info::generate(self.ctxt, self.fct);

        if self.ctxt.args.flag_emit_debug {
            emit::debug(&mut self.buf);
        }

        self.emit_prolog();
        self.store_register_params_on_stack();
        self.visit_fct(self.ast);

        let always_returns = self.fct.src().always_returns;

        if !always_returns {
            self.emit_epilog();
        }

        let jit_fct = self.buf.jit(self.fct.id);

        let mut code_map = self.ctxt.code_map.lock().unwrap();
        code_map.insert(jit_fct.ptr_start(), jit_fct.ptr_end(), jit_fct.fct_id());

        jit_fct
    }

    fn store_register_params_on_stack(&mut self) {
        let hidden_self = if self.fct.ctor {
            let var = self.fct.var_self();
            emit::mov_reg_local(&mut self.buf, var.ty.mode(),
                                REG_PARAMS[0], var.offset);

            1

        } else {
            0
        };

        for (&reg, p) in REG_PARAMS.iter().skip(hidden_self)
                        .zip(&self.ast.params) {
            var_store(&mut self.buf, self.fct, reg, p.var());
        }
    }

    fn emit_prolog(&mut self) {
        emit::prolog(&mut self.buf, self.fct.src().stacksize());
    }

    fn emit_epilog(&mut self) {
        emit::epilog(&mut self.buf, self.fct.src().stacksize());
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);
        }

        // finally block is currently active, plain return is not allowed, finally block
        // needs to be executed
        if let Some(lbl_finally) = self.lbl_finally {
            // store return value if available
            let status: TryStatus = if s.expr.is_some() {
                emit::mov_reg_local(&mut self.buf, self.fct.return_type.mode(),
                                    REG_RESULT, self.fct.src().eh_return_value.unwrap());

                TryStatus::ReturnValue
            } else {
                TryStatus::Return
            };

            emit::movl_imm_reg(&mut self.buf, status.code(), REG_RESULT);
            emit::mov_reg_local(&mut self.buf, MachineMode::Int32,
                                REG_RESULT, self.fct.src().eh_status.unwrap());
            emit::jump(&mut self.buf, lbl_finally);

        // if no finally-block currently active just exit from the current function
        } else {
            self.emit_epilog();
        }
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.buf.define_label(lbl_start);

        if s.cond.is_lit_true() {
            // always true => no condition evaluation

        } else {
            // execute condition, when condition is false jump to
            // end of while
            let reg = self.emit_expr(&s.cond);
            emit::jump_if(&mut self.buf, JumpCond::Zero, reg, lbl_end);
        }

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);
            emit::jump(&mut this.buf, lbl_start);
        });

        self.buf.define_label(lbl_end);
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();
        self.buf.define_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);
            emit::jump(&mut this.buf, lbl_start);
        });

        self.buf.define_label(lbl_end);
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
        emit::jump_if(&mut self.buf, JumpCond::Zero, reg, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            emit::jump(&mut self.buf, lbl_end);
            self.buf.define_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.buf.define_label(lbl_end);
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

    fn emit_stmt_let(&mut self, s: &'ast StmtLetType) {
        let mut initialized = false;

        if let Some(ref expr) = s.expr {
            let reg = self.emit_expr(expr);
            initialized = true;

            var_store(&mut self.buf, self.fct, reg, s.var());
        }

        let reference_type = {
            let var = self.fct.var(s.var());

            if var.ty.reference_type() {
                self.scopes.add_var(var.id, var.offset);
            }

            var.ty.reference_type()
        };

        // uninitialized variables which reference objects need to be initialized to null
        // otherwise the GC  can't know if the stored value is a valid pointer
        if reference_type && !initialized {
            emit::nil(&mut self.buf, REG_RESULT);
            var_store(&mut self.buf, self.fct, REG_RESULT, s.var());
        }
    }

    fn emit_stmt_throw(&mut self, s: &'ast StmtThrowType) {
        let reg = self.emit_expr(&s.expr);
        emit::nil_ptr_check(&mut self.buf, s.pos, reg);
        emit::mov_reg_reg(&mut self.buf, MachineMode::Ptr, reg, REG_PARAMS[0]);

        let ptr = Ptr::new(stdlib::throw_exception as *mut libc::c_void);
        let disp = self.buf.add_addr(ptr);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(&mut self.buf, disp + pos, REG_RESULT);
        emit::call(&mut self.buf, REG_RESULT);
    }

    fn emit_stmt_try(&mut self, s: &'ast StmtTryType) {
        let saved_lbl_finally = self.lbl_finally;
        let lbl_finally = self.buf.create_label();

        // if finally block given then use label as current finally
        // otherwise lbl_finally is just used at label after all catch blocks
        if s.finally_block.is_some() {
            self.lbl_finally = Some(lbl_finally);
        }

        let try_start = self.buf.pos();
        self.visit_stmt(&s.try_block);

        emit::movl_imm_reg(&mut self.buf, TryStatus::Finished.code(), REG_RESULT);
        emit::mov_reg_local(&mut self.buf, MachineMode::Int32,
                            REG_RESULT, self.fct.src().eh_status.unwrap());
        emit::jump(&mut self.buf, lbl_finally);
        let try_end = self.buf.pos();

        for catch in &s.catch_blocks {
            // TODO: check class of exception object

            self.visit_stmt(&catch.block);
            emit::jump(&mut self.buf, lbl_finally);
        }

        self.buf.define_label(lbl_finally);
        self.lbl_finally = saved_lbl_finally;

        if let Some(ref finally_block) = s.finally_block {
            self.visit_stmt(finally_block);

            // check TryStatus::Return
            self.emit_try_return_check(false);

            // check TryStatus::ReturnValue
            self.emit_try_return_check(true);

            // check TryStatus::NoMatchingCatch
            // TODO
        }

        // stores offsets of try- and catch-block in buffer
        self.buf.add_exception_handler(try_start, try_end, try_end);
    }

    fn emit_try_return_check(&mut self, with_value: bool) {
        let lbl_after = self.buf.create_label();
        let status = if with_value { TryStatus::ReturnValue } else { TryStatus::Return };

        emit::movl_imm_reg(&mut self.buf, status.code(), REG_RESULT);
        emit::mov_local_reg(&mut self.buf, MachineMode::Int32,
                            self.fct.src().eh_status.unwrap(), REG_TMP1);
        emit::cmp_setl(&mut self.buf, MachineMode::Ptr, REG_RESULT,
                       CmpOp::Eq, REG_TMP1, REG_RESULT);
        emit::jump_if(&mut self.buf, JumpCond::Zero, REG_RESULT, lbl_after);

        if with_value {
            emit::mov_local_reg(&mut self.buf, self.fct.return_type.mode(),
                                self.fct.src().eh_return_value.unwrap(), REG_RESULT);
        }

        self.emit_epilog();
        self.buf.define_label(lbl_after);
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> Reg {
        let expr_gen = ExprGen::new(self.ctxt, self.fct, self.ast,
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
            StmtLet(ref stmt) => self.emit_stmt_let(stmt),
            StmtThrow(ref stmt) => self.emit_stmt_throw(stmt),
            StmtTry(ref stmt) => self.emit_stmt_try(stmt),
        }
    }

    fn visit_expr(&mut self, _: &'ast Expr) {
        unreachable!("should not be invoked");
    }
}

pub enum JumpCond {
    Zero,
    NonZero
}

pub fn var_store(buf: &mut Buffer, fct: &Fct, src: Reg, var_id: VarId) {
    let var = fct.var(var_id);
    emit::mov_reg_local(buf, var.ty.mode(), src, var.offset);
}

pub fn var_load(buf: &mut Buffer, fct: &Fct, var_id: VarId, dest: Reg) {
    let var = fct.var(var_id);
    emit::mov_local_reg(buf, var.ty.mode(), var.offset, dest);
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
}

struct Scope {
    vars: HashMap<VarId, i32>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            vars: HashMap::new()
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
        for (var, &offset) in &scope.vars {
            offsets.push(offset);
        }
    }

    for &offset in &temps.offsets {
        offsets.push(offset);
    }

    GcPoint::from_offsets(offsets)
}

#[cfg(test)]
mod tests {
    use std::mem;

    use driver;
    use driver::cmd::AsmSyntax;
    use jit;
    use jit::buffer::Buffer;
    use jit::codegen::{CodeGen, Scopes};
    use jit::fct::JitFct;
    use mem::ptr::Ptr;
    use test;

    fn compile(code: &'static str) -> JitFct {
        test::parse(code, |ctxt| {
            let fct_name = "f";

            let name = ctxt.interner.intern("f");
            let fctid = ctxt.sym.borrow().get_fct(name).unwrap();

            ctxt.fct_by_id_mut(fctid, |fct| {
                let ast = fct.ast();

                let mut cg = CodeGen {
                    ctxt: ctxt,
                    fct: fct,
                    ast: ast,
                    buf: Buffer::new(),
                    scopes: Scopes::new(),

                    lbl_break: None,
                    lbl_continue: None,
                    lbl_finally: None,
                };

                cg.generate()
            })
        })
    }

    fn run<T>(code: &'static str) -> T {
        let m = compile(code);
        let compiled_fct : extern "C" fn() -> T = unsafe {
            mem::transmute(m.fct_ptr())
        };

        compiled_fct()
    }

    fn fct1<T>(code: &'static str) -> (JitFct, extern "C" fn(T) -> T) {
        let m = compile(code);
        let fct = m.fct_ptr();

        (m, unsafe { mem::transmute(fct) })
    }

    fn fct2<T>(code: &'static str) -> (JitFct, extern "C" fn(T, T) -> T) {
        let m = compile(code);
        let fct = m.fct_ptr();

        (m, unsafe { mem::transmute(fct) })
    }

    #[test]
    fn test_lit_int() {
        assert_eq!(1i32, run("fn f() -> int { return 1; }"));
        assert_eq!(2i32, run("fn f() -> int { return 2; }"));
        assert_eq!(3i32, run("fn f() -> int { return 3; }"));
    }

    #[test]
    fn test_expr_un_plus() {
        let (mem, f) = fct1("fn f(a: int) -> int { return +a; }");

        assert_eq!(-1i32, f(-1));
        assert_eq!(0, f(0));
        assert_eq!(1, f(1));
    }

    #[test]
    fn test_expr_un_neg() {
        let (mem, f) = fct1("fn f(a: int) -> int { return -a; }");

        assert_eq!(1i32, f(-1));
        assert_eq!(0, f(0));
        assert_eq!(-1, f(1));
    }

    #[test]
    fn test_expr_un_bit_not() {
        let (mem, f) = fct1("fn f(a: int) -> int { return ~a; }");

        assert_eq!(0i32, f(-1));
        assert_eq!(-1, f(0));
        assert_eq!(-2, f(1));
    }

    #[test]
    fn test_expr_un_not() {
        let (mem, f) = fct1("fn f(a: bool) -> bool { return !a; }");

        assert_eq!(true, f(false));
        assert_eq!(false, f(true));
    }

    #[test]
    fn test_param() {
        let (mem, f) = fct1("fn f(a: int) -> int { return a; }");

        assert_eq!(0i32, f(0));
        assert_eq!(1, f(1));
        assert_eq!(2, f(2));
    }

    #[test]
    fn test_param_bool() {
        let (mem, f) = fct1("fn f(a: bool) -> bool { return a; }");

        assert_eq!(true, f(true));
        assert_eq!(false, f(false));
    }

    #[test]
    fn test_lit_bool() {
        assert_eq!(true, run("fn f() -> bool { return true; }"));
        assert_eq!(false, run("fn f() -> bool { return false; }"));
    }

    #[test]
    fn test_if() {
        assert_eq!(1i32, run("fn f() -> int {
                                if true { return 1; } else { return 2; } }"));
        assert_eq!(1i32, run("fn f() -> int {
                                if true { return 1; } return 2; }"));
        assert_eq!(2i32, run("fn f() -> int {
                                if false { return 1; } else { return 2; } }"));
        assert_eq!(2i32, run("fn f() -> int {
                                if false { return 1; } return 2; }"));
    }

    #[test]
    fn test_ident_load_and_store() {
        assert_eq!(4711, run("fn f() -> int { let a = 4711; return a; }"));
        assert_eq!(true, run("fn f() -> bool { let a = true; return a; }"));
        assert_eq!(false, run("fn f() -> bool { let a = false; return a; }"));
    }

    #[test]
    fn test_assign() {
        assert_eq!(4711, run("fn f() -> int {
                                let a: int; a = 4711; return a; }"));
        assert_eq!(true, run("fn f() -> bool {
                                let a: bool; a = true; return a; }"));
        assert_eq!(false, run("fn f() -> bool {
                                 let a: bool; a = false; return a; }"));
    }

    #[test]
    fn test_add() {
        assert_eq!(3, run("fn f() -> int {
                             let a = 1; let b = 2;
                             return a + b; }"));
    }

    #[test]
    fn test_add_with_complex_rhs() {
        assert_eq!(10, run("fn f() -> int { return 1+(2+(3+4)); }"));
    }

    #[test]
    fn test_sub() {
        assert_eq!(-1, run("fn f() -> int {
                              let a = 1; let b = 2;
                              return a - b; }"));
    }

    #[test]
    fn test_sub_with_complex_rhs() {
        assert_eq!(-1, run("fn f() -> int { return 3-4; }"));
        assert_eq!(3, run("fn f() -> int { return 2-(3-4); }"));
        assert_eq!(-2, run("fn f() -> int { return 1-(2-(3-4)); }"));
        assert_eq!(42, run("fn f() -> int {
            let a = 7;
            let b = 1+(2+3);

            return a*b;
        }"))
    }

    #[test]
    fn test_bit_or() {
        assert_eq!(3, run("fn f() -> int {
                             let a = 1; let b = 2;
                             return a | b; }"));
    }

    #[test]
    fn test_bit_and() {
        assert_eq!(1, run("fn f() -> int {
                             let a = 1; let b = 3;
                             return a & b; }"));
    }

    #[test]
    fn test_bit_xor() {
        assert_eq!(1, run("fn f() -> int {
                             let a = 3; let b = 2;
                             return a ^ b; }"));
    }

    #[test]
    fn test_mul() {
        assert_eq!(6, run("fn f() -> int {
                             let a = 3; let b = 2;
                             return a * b; }"));
    }

    #[test]
    fn test_div() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               let b = 3; return a / b; }");
        let b = 3;

        for a in 0..8 {
            assert_eq!(a/b, f(a));
        }
    }

    #[test]
    fn test_mod() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               let b = 3; return a % b; }");
        let b = 3;

        for a in 0..8 {
            assert_eq!(a%b, f(a));
        }
    }

    #[test]
    fn test_cmp_lt() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a < 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(0, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_le() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a <= 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(1, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_gt() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a > 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(0, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_cmp_ge() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a >= 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(1, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_cmp_eq() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a == 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(1, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_ne() {
        let (mem, f) = fct1("fn f(a: int) -> int {
                               if a != 3 { return 1; }
                               else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(0, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_or() {
        let (mem, f) = fct2("fn f(a: bool, b: bool) -> bool {
                               return a || b; }");

        assert_eq!(true, f(true, true));
        assert_eq!(true, f(true, false));
        assert_eq!(true, f(false, true));
        assert_eq!(false, f(false, false));
    }

    #[test]
    fn test_and() {
        let (mem, f) = fct2("fn f(a: bool, b: bool) -> bool {
                               return a && b; }");

        assert_eq!(true, f(true, true));
        assert_eq!(false, f(true, false));
        assert_eq!(false, f(false, true));
        assert_eq!(false, f(false, false));
    }
}
