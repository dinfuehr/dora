use std::collections::HashMap;
use std::mem;

use ast::{Ast, Expr, Stmt, Param};
use ast::{ExprAssignType, ExprIdentType, ExprBinType, ExprUnType};
use ast::{ExprLitStrType, ExprLitIntType, ExprLitBoolType};
use ast::{StmtVarType, StmtExprType, StmtIfType, StmtReturnType};
use ast::{StmtLoopType, StmtWhileType, StmtBlockType, Function};
use ast::Expr::*;
use ast::Stmt::*;
use ast::visit::*;

use ctxt::*;

use ir;
use ir::*;
use ir::Instr::*;
use ir::Opnd::*;

use sym::BuiltinType;

pub fn generate<'a, 'ast>(ctxt: &Context<'a, 'ast>, fct: &'ast Function) {
    Generator::new(ctxt, fct).generate();
}

struct Generator<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    vreg: u32,
    result: Opnd,
    block_id: BlockId,
    join_id: BlockId,
    ast_fct: &'ast Function,
    ir: Fct,
    var_map: HashMap<VarInfoId, VarId>
}

impl<'a, 'ast> Generator<'a, 'ast> {
    fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> Generator<'a, 'ast> {
        Generator {
            ctxt: ctxt,
            vreg: 0,
            result: OpndInt(0),
            block_id: BlockId(0),
            join_id: BlockId(0),
            ast_fct: fct,
            ir: Fct::new(),
            var_map: HashMap::new(),
        }
    }

    fn generate(&mut self) {
        for p in &self.ast_fct.params {
            self.visit_param(p);
        }

        self.block_id = self.ir.add_block();
        self.visit_stmt(&self.ast_fct.block);

        self.ensure_return();

        let ir = mem::replace(&mut self.ir, Fct::new());
        ir::dump::dump(self.ctxt, &ir);

        self.ctxt.fct_info_mut(self.ast_fct.id, |fct| fct.ir = Some(ir));
    }

    fn add_stmt_var(&mut self, stmt: &'ast StmtVarType) {
        let var_id = self.ctxt.var(stmt.id, |ctxt_var, ctxt_var_id| {
            let ir_var_id = self.ir.add_var(stmt.name, ctxt_var.data_type, 0);
            self.var_map.insert(ctxt_var_id, ir_var_id);

            ir_var_id
        });

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);

            let src = self.result;
            self.add_instr_assign_var(var_id, src);
        }
    }

    fn add_stmt_expr(&mut self, stmt: &'ast StmtExprType) {
        self.visit_expr(&stmt.expr);
    }

    fn add_stmt_if(&mut self, stmt: &'ast StmtIfType) {
        let before_id = self.block_id;
        let then_id = self.ir.add_block();
        let after_id = self.ir.add_block();
        let else_id = if stmt.else_block.is_some() {
            self.ir.add_block()
        } else {
            after_id
        };

        self.visit_expr(&stmt.cond);
        let result = self.result;
        self.add_instr(Instr::test(result, then_id, else_id));

        self.block_id = then_id;
        self.visit_stmt(&stmt.then_block);
        self.add_instr(Instr::goto(after_id));
        self.block_mut().add_predecessor(before_id);

        if let Some(ref else_block) = stmt.else_block {
            self.block_id = else_id;
            self.visit_stmt(else_block);
            self.add_instr(Instr::goto(after_id));
            self.block_mut().add_predecessor(before_id);
        }

        self.block_id = after_id;
    }

    fn add_stmt_loop(&mut self, stmt: &'ast StmtLoopType) {
        let before_id = self.block_id;
        let loop_id = self.ir.add_block();
        let after_id = self.ir.add_block();

        self.add_instr(InstrGoto(loop_id));

        self.block_id = loop_id;
        self.visit_stmt(&stmt.block);
        self.add_instr(Instr::goto(loop_id));
        self.block_mut().add_predecessor(before_id);

        self.block_id = after_id;
    }

    fn add_stmt_while(&mut self, stmt: &'ast StmtWhileType) {

    }

    fn add_stmt_block(&mut self, stmt: &'ast StmtBlockType) {
        for s in &stmt.stmts {
            self.visit_stmt(s);
        }
    }

    fn add_stmt_return(&mut self, stmt: &'ast StmtReturnType) {
        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);

            let result = self.result;
            self.add_instr(Instr::ret_with(result));
        } else {
            self.add_instr(Instr::ret());
        }
    }

    fn add_expr_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        self.result = OpndInt(lit.value);
    }

    fn add_expr_lit_bool(&mut self, lit: &'ast ExprLitBoolType) {
        self.result = OpndBool(lit.value);
    }

    fn add_expr_lit_str(&mut self, lit: &'ast ExprLitStrType) {
        let dest = self.next_vreg();
        let instr = Instr::str(dest, lit.value.clone());
        self.add_instr(instr);

        self.result = dest;
    }

    fn add_expr_un(&mut self, expr: &'ast ExprUnType) {
        self.visit_expr(&expr.opnd);
        let src = self.result;

        let dest = self.next_vreg();
        let instr = Instr::un(dest, expr.op, src);
        self.add_instr(instr);
        self.result = dest;
    }

    fn add_expr_bin(&mut self, expr: &'ast ExprBinType) {
        self.visit_expr(&expr.lhs);
        let lhs = self.result;

        self.visit_expr(&expr.rhs);
        let rhs = self.result;

        let dest = self.next_vreg();
        let instr = Instr::bin(dest, lhs, expr.op, rhs);
        self.add_instr(instr);
        self.result = dest;
    }

    fn add_expr_ident(&mut self, expr: &'ast ExprIdentType) {
        let var_id = self.ctxt.var(expr.id, |_, ctxt_var_id| {
            *self.var_map.get(&ctxt_var_id).unwrap()
        });

        let ssa_index = self.ir.vars[var_id.0].ssa_index;
        self.result = OpndVar(var_id, ssa_index);
    }

    fn add_expr_assign(&mut self, expr: &'ast ExprAssignType) {
        let var_id = self.ctxt.var(expr.lhs.id(), |_, ctxt_var_id| {
            *self.var_map.get(&ctxt_var_id).unwrap()
        });

        self.visit_expr(&expr.rhs);
        let src = self.result;

        self.add_instr_assign_var(var_id, src);
    }

    fn ensure_return(&mut self) {
        if let Some(&InstrRet(_)) = self.block().last_instr() {
            // already ends with ret: do nothing
        } else if self.ctxt.fct_info(self.ast_fct.id, |fct| fct.return_type) == BuiltinType::Unit {
            self.add_instr(Instr::ret());
        }
    }

    fn add_instr_assign_var(&mut self, dest: VarId, src: Opnd) {
        let ssa_index = {
            let var = &mut self.ir.vars[dest.0];
            var.ssa_index += 1;

            var.ssa_index
        };

        self.add_instr_assign(OpndVar(dest, ssa_index), src);
    }

    fn add_instr_assign(&mut self, dest: Opnd, src: Opnd) {
        self.add_instr(Instr::assign(dest, src));
    }

    fn add_instr(&mut self, instr: Instr) {
        self.block_mut().add_instr(instr);
    }

    fn join_mut(&mut self) -> &mut Block {
        self.ir.block_mut(self.join_id)
    }

    fn block_mut(&mut self) -> &mut Block {
        self.ir.block_mut(self.block_id)
    }

    fn block(&self) -> &Block {
        self.ir.block(self.block_id)
    }

    fn next_vreg(&mut self) -> Opnd {
        let vreg = self.vreg;

        self.vreg += 1;

        OpndReg(vreg)
    }
}

impl<'a, 'ast> Visitor<'ast> for Generator<'a, 'ast> {
    fn visit_param(&mut self, p: &'ast Param) {
        self.ctxt.var(p.id, |ctxt_var, ctxt_var_id| {
            let ir_var_id = self.ir.add_var(p.name, ctxt_var.data_type, 0);
            self.var_map.insert(ctxt_var_id, ir_var_id);
        });
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.add_stmt_expr(stmt),
            StmtIf(ref stmt) => self.add_stmt_if(stmt),
            StmtLoop(ref stmt) => self.add_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.add_stmt_while(stmt),
            StmtBlock(ref stmt) => self.add_stmt_block(stmt),
            StmtVar(ref stmt) => self.add_stmt_var(stmt),
            StmtReturn(ref stmt) => self.add_stmt_return(stmt),
            _ => panic!("unsupported statement")
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.add_expr_lit_int(expr),
            ExprLitBool(ref expr) => self.add_expr_lit_bool(expr),
            ExprLitStr(ref expr) => self.add_expr_lit_str(expr),
            ExprIdent(ref expr) => self.add_expr_ident(expr),
            ExprUn(ref expr) => self.add_expr_un(expr),
            ExprBin(ref expr) => self.add_expr_bin(expr),
            ExprAssign(ref expr) => self.add_expr_assign(expr),
            _ => panic!("unsupported expression")
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{BinOp, UnOp};
    use ctxt::*;
    use ir;
    use ir::*;
    use ir::Instr::*;
    use ir::Opnd::*;
    use test::parse;

    fn check_fct<F, T>(code: &'static str, fname: &'static str, f: F) -> T where F: FnOnce(&Context, &FctInfo) -> T {
        parse(code, |ctxt| {
            let name = ctxt.interner.intern(fname);
            let fct_info_id = ctxt.sym.borrow().get_function(name).unwrap();

            let fct = ctxt.fct_info_for_id(fct_info_id, |fct_info| fct_info.ast.unwrap());
            ir::gen::generate(ctxt, fct);

            ctxt.fct_info_for_id(fct_info_id, |fct_info| f(ctxt, fct_info))
        })
    }

    #[test]
    fn assign() {
        check_fct("fn f() { var x = 1; x = x + 2; x = x + 3; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(6, instrs.len());
            assert_eq!(Instr::assign(OpndVar(VarId(0), 1), OpndInt(1)), instrs[0]);

            let lhs = OpndVar(VarId(0), 1);
            let rhs = OpndInt(2);
            assert_eq!(Instr::bin(OpndReg(0), lhs, BinOp::Add, rhs), instrs[1]);

            assert_eq!(Instr::assign(OpndVar(VarId(0), 2), OpndReg(0)), instrs[2]);

            let lhs = OpndVar(VarId(0), 2);
            let rhs = OpndInt(3);
            assert_eq!(Instr::bin(OpndReg(1), lhs, BinOp::Add, rhs), instrs[3]);

            assert_eq!(Instr::assign(OpndVar(VarId(0), 3), OpndReg(1)), instrs[4]);

            assert_eq!(Instr::ret(), instrs[5]);
        });
    }

    #[test]
    fn return_value() {
        check_fct("fn f() -> int { return 1; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(1, instrs.len());
            assert_eq!(Instr::ret_with(OpndInt(1)), instrs[0]);
        });
    }

    #[test]
    fn return_without_value() {
        check_fct("fn f() { return; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(1, instrs.len());
            assert_eq!(Instr::ret(), instrs[0]);
        });
    }

    #[test]
    fn implicit_return() {
        check_fct("fn f() { }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(1, instrs.len());
            assert_eq!(Instr::ret(), instrs[0]);
        });
    }

    #[test]
    fn param() {
        check_fct("fn f(a: int) -> int { return a; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            let param = OpndVar(VarId(0), 0);
            assert_eq!(Instr::ret_with(param), instrs[0]);
        });
    }

    #[test]
    fn expr() {
        check_fct("fn f() { 1; 2; 3; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(1, instrs.len());
            assert_eq!(Instr::ret(), instrs[0]);
        });
    }

    #[test]
    fn bin() {
        check_fct("fn f(a: int, b: int) -> int { return a+b; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(2, instrs.len());

            let a = OpndVar(VarId(0), 0);
            let b = OpndVar(VarId(1), 0);
            let temp = OpndReg(0);
            assert_eq!(Instr::bin(temp, a, BinOp::Add, b), instrs[0]);
            assert_eq!(Instr::ret_with(temp), instrs[1]);
        });
    }

    #[test]
    fn un() {
        check_fct("fn f(a: int) -> int { return -a; }", "f", |ctxt, fct| {
            let ir_fct = fct.ir.as_ref().unwrap();
            assert_eq!(1, ir_fct.blocks.len());

            let instrs = &ir_fct.blocks[0].instructions;
            assert_eq!(2, instrs.len());

            let a = OpndVar(VarId(0), 0);
            let temp = OpndReg(0);
            assert_eq!(Instr::un(temp, UnOp::Neg, a), instrs[0]);
            assert_eq!(Instr::ret_with(temp), instrs[1]);
        });
    }
}
